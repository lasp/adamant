import os.path
from util import debug
import shutil

# This module provides common filesystem related functions.


# Make a directory, and its parents, if it does not already
# exist. If the directory already exists, don't complain.
def safe_makedir(directory):
    import pathlib

    pathlib.Path(directory).mkdir(parents=True, exist_ok=True)


def safe_symlink(filename, link_filename, overwrite=False):
    def do_link():
        try:
            debug.debug_print("ln -s " + filename + " " + link_filename)
            os.symlink(filename, link_filename)
        except OSError:
            # If the host machine does not allow symlinks, ie. Windows, then
            # let's just copy the file instead.
            debug.debug_print("cp " + filename + " " + link_filename)
            shutil.copyfile(filename, link_filename)

    if filename != link_filename:
        if os.path.islink(link_filename):
            if overwrite:
                os.remove(link_filename)
                do_link()
        else:
            do_link()


# This generator is a modified version of os.walk, except that it
# ignores "build" and "alire" directories (by default) and hidden directories
# as it recurses. This improves performance of a recursive search.
def recurse_through_repo(directory, ignore=["build", "alire"]):
    # Walk recursively through the repository, ignoring hidden directories and build directories:
    for root, dirnames, filenames in os.walk(directory):
        # Don't traverse into hidden directories:
        dirnames[:] = [
            d
            for d in dirnames
            if not d[0] == "." and not d[0] == "_" and d not in ignore
        ]
        yield root, dirnames, filenames


# Return a list of all the files found in a directory.
def get_files_in_dir(directory):
    files = []
    for f in os.listdir(directory):
        full_file = os.path.join(directory, f)
        if os.path.isfile(full_file):
            files.append(full_file)
    return files
