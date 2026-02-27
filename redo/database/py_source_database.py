from database.database import database
from database.database import DATABASE_MODE
from database import util
from util import error
import os.path


class py_source_database(database):
    """
    This database is responsible for storing python source files found in
    the build path. The database maps the python module name to the source
    code of that module, as well as any model files that
    are used to generate that source code. An example is shown below:

    Key:             Value:
    module_name      ([/path/to/module_name.py, \
    /path/to/module_name.yaml)

    This database is useful for figuring out where python is located
    in the system given just a module name (which is all that is included
    in the import dependencies of a python source file).
    """
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(py_source_database, self).__init__(
            util.get_database_file("py_source"), mode
        )

    def insert_source(self, source_filename, model_filename=None):
        """
        Insert a source file into the database (and the associated model file, if
        there is one). Note that for a given module name (which is derived from the
        passed in source code) only a single .py
        If adding another file is attempted, the function will error and warn the user
        that every source in the build path must have a unique filename, and thus a
        unique model name.
        """
        # Error functions:
        def _duplicate_source_error(file1, file2):
            error.error_abort(
                ("All python source in the build path must have unique module and file names. "
                 "The following two source files conflict: [")
                + file1
                + ", "
                + file2
                + "]"
            )

        def _duplicate_model_error(file1, file2):
            error.error_abort(
                ("All models in the build path must produce unique module and file names. "
                 "The following two model files conflict: [")
                + file1
                + ", "
                + file2
                + "] because they both produce a file called '"
                + os.path.basename(source_filename)
                + "'"
            )

        # Get the module name from the filename and try to
        # fetch a record if it exists.
        module_name = os.path.splitext(os.path.basename(source_filename))[0]
        record = self.try_fetch(module_name)

        # If the record already exists, we need to be very careful about adding another source
        # file. It must be the complement to the source file that already exists. For example,
        # if hello.adb exists, we can add hello.ads, but nothing else. If we find another piece of
        # source, then we potentially have two different modules named the same thing, and we
        # should warn the user.
        if record:
            # Make sure that the models don't conflict:
            existing_model = record[1]
            if existing_model and model_filename and existing_model != model_filename:
                _duplicate_model_error(existing_model, model_filename)

            # We have a specific conflict:
            _duplicate_source_error(source_filename, record[0])
        else:
            record = [source_filename, model_filename]

        # Insert record into database:
        self.store(module_name, record)

    def get_source(self, module_name):
        """Given a module name, return the associated source files."""
        return self.fetch(module_name)[0]

    def try_get_source(self, module_name):
        """
        Given a module name, try to return the associated source files
        otherwise return an empty list.
        """
        try:
            return self.get_source(module_name)
        except KeyError:
            return None

    def get_sources(self, module_names):
        """Given a list of module names, return the associated source files."""
        sources = []
        if isinstance(module_names, str):
            module_names = [module_names]
        for name in module_names:
            source = self.get_source(name)
            if source:
                sources.append(source)
        return list(sources)

    def try_get_sources(self, module_names):
        """Given a list of module names, try to return the associated source files."""
        sources = []
        if isinstance(module_names, str):
            module_names = [module_names]
        for name in module_names:
            source = self.try_get_source(name)
            if source:
                sources.append(source)
        return list(sources)

    def get_model(self, module_name):
        """Given a module name, return the associated model file."""
        source_record = self.try_fetch(module_name)
        if source_record:
            return source_record[1]
        return None
