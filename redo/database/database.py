import unqlite
import pickle
import os
import time
from enum import Enum
from filelock import FileLock, Timeout

# Large recursive items sometimes fail to pickle due to reaching the
# recursion limit. Let's increase that to something more reasonable
# here.
import sys

sys.setrecursionlimit(5000)

# This module provides a thin wrapper around an unqlite
# database. Because unqlite can only store textual values,
# the database class provided in this module stores the
# pickled version of python objects. In this way, any
# data type can be stored within the underlying unqlite
# database.

# UNQLITE Constant Definitions. For some reason
# I cannot find this in the unqlite module, so I
# have just redefined them here.
_UNQLITE_OPEN_READONLY = 0x00000001
_UNQLITE_OPEN_READWRITE = 0x00000002
_UNQLITE_OPEN_CREATE = 0x00000004
_UNQLITE_OPEN_EXCLUSIVE = 0x00000008
_UNQLITE_OPEN_TEMP_DB = 0x00000010
_UNQLITE_OPEN_NOMUTEX = 0x00000020
_UNQLITE_OPEN_OMIT_JOURNALING = 0x00000040
_UNQLITE_OPEN_IN_MEMORY = 0x00000080
_UNQLITE_OPEN_MMAP = 0x00000100


# Init modes. This enumeration tells the database
# class below what permissions a database object
# should have. A "read_only" database object will be more
# performant than a "read_write" or "create" database
# object, so it should be preferred when possible.
class DATABASE_MODE(Enum):
    READ_ONLY = 0
    READ_WRITE = 1
    CREATE = 2


def _get_flock_filename(filename):
    return filename + ".lck"


def _get_flock(filename):
    return FileLock(_get_flock_filename(filename), timeout=10)


# Private helper functions for deleting/opening/creating an unqlite database:
def _destroy(filename):
    try:
        os.remove(filename)
    except OSError:
        pass
    try:
        os.remove(_get_flock_filename(filename))
    except OSError:
        pass


def _create(filename):
    # Create a database from a fresh file every time. If the user
    # does not want this they can call open_rw instead.
    _destroy(filename)
    lock = _get_flock(filename)
    db = None
    try:
        with lock:
            db = unqlite.UnQLite(filename, flags=_UNQLITE_OPEN_CREATE)
    except Timeout:
        raise Exception(
            "(create) Failed to grab lock for the database file: " + filename
        )
    return db, lock


def _open_ro(filename):
    return unqlite.UnQLite(filename, flags=_UNQLITE_OPEN_READONLY), None


def _open_rw(filename):
    lock = _get_flock(filename)
    db = None
    try:
        with lock:
            db = unqlite.UnQLite(filename, flags=_UNQLITE_OPEN_READWRITE)
    except Timeout:
        raise Exception(
            "(open_rw) Failed to grab lock for the database file: " + filename
        )
    return db, lock


# Sometimes unqlite complains when you try to read from a database with
# an exclusive lock. I think this happens when a database is currently
# being created, while another thread tries to read. In this case
# let's try 10 times before failing, sleeping a bit more each time.
def _try_try_again(func):
    count = 0
    while True:
        try:
            return func()
        except unqlite.UnQLiteError as e:
            if count >= 10:
                raise e
            count += 1
            time.sleep(count * 0.05)


# This is a basic database object which allows key/value storage
# where the storage type can be any python data structure. Under
# the hood it uses pickle to store python datastructures in an
# unqlite NoSQL database.
class database(object):
    # Initialize the database object. It can be initialized in
    # 3 different modes: read_only, read_write, and create.
    # read_only is the default mode, and the most performant.
    # create will destroy the old database file before instantiating
    # a new one.
    def __init__(self, filename, mode=DATABASE_MODE.READ_ONLY):
        self.filename = filename
        if mode == DATABASE_MODE.READ_ONLY:
            self.db, self.lock = _open_ro(filename)
        elif mode == DATABASE_MODE.READ_WRITE:
            self.db, self.lock = _open_rw(filename)
        elif mode == DATABASE_MODE.CREATE:
            self.db, self.lock = _create(filename)
        else:
            raise ValueError(
                "mode must be set to either READ_ONLY, READ_WRITE, or CREATE."
            )

    # Close the database object. Note that because of the
    # __enter__/__exit__ methods below, using a "with"
    # statement with the database should be preferred to calling
    # this close method manually.
    def close(self):
        try:
            self.db.close()
        except BaseException:
            pass

    # Completely remote the database from the filesystem.
    def destroy(self):
        self.close()
        _destroy(self.filename)

    # Close the database.
    def __del__(self):
        self.close()

    # Enter function for a python "with" statement.
    def __enter__(self):
        return self

    # The exit function for a python "with" statement
    # automatically handles closing the database.
    def __exit__(self, type, value, traceback):
        self.close()

    # Store data in the database for a specific string key
    def store(self, key, data):
        def _do_store():
            try:
                # We must have mutual exclusion on writes, so we use an external
                # file lock to do so since unqlite does not provide this feature
                # itself.
                with self.lock:
                    # Serialize the data and store it in the database:
                    sdata = pickle.dumps(data, protocol=pickle.HIGHEST_PROTOCOL)
                    self.db[key] = sdata
            except Timeout:
                raise Exception(
                    "Failed to grab lock for the database file: " + self.filename
                )

        _try_try_again(_do_store)

    # Extract data from the database for a specific string
    # key. If the key does not exist, throw a KeyError
    # exception.
    def fetch(self, key):
        # Extract data from database and deserialize it into
        # a python data structure:
        def _do_fetch():
            try:
                sdata = self.db[key]
                return pickle.loads(sdata)
            except KeyError:
                raise KeyError(
                    "No key '" + key + "' exists in database. fetch() failed."
                )

        return _try_try_again(_do_fetch)

    # Extract data from the database for a specific string
    # key. If the key does not exist, None is returned.
    def try_fetch(self, key):
        # Extract data from database and deserialize it into
        # a python data structure:
        def _do_try_fetch():
            try:
                sdata = self.db[key]
                return pickle.loads(sdata)
            except KeyError:
                return None

        return _try_try_again(_do_try_fetch)

    # Return a list of all the keys that exist in the database.
    def keys(self):
        return list(self.db.keys())

    # Return a list of all the values that exist in the database.
    def values(self):
        keys = self.keys()
        values = []
        for key in keys:
            values.append(self.fetch(key))
        return values

    # Return True is a key exists in the database, otherwise
    # return False.
    def does_key_exist(self, key):
        return key in self.db

    # Convert the entire database into a human readable
    # string representation.
    def __repr__(self):
        string = ""
        for key, value in self.db.items():
            data = pickle.loads(value)
            string += str(key) + " : " + str(data) + "\n"
        return string

    # Convert the entire database into a human readable
    # string representation.
    def __str__(self):
        return self.__repr__()
