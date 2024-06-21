import importlib.util
import os.path
import sys

# This module provides useful meta programming tasks for python.


def get_modules_in_dir(dir_names=[]):
    """
    Given a list of directory names this function will search the python path
    for those directory names. If a directory with that name is found in the path
    as an immediate subdirectory, all python modules found within
    that subdirectory are returned. For example, this function can be used to find
    all python modules within the PYTHONPATH that exist in a subdirectory called
    "test" or "tests"
    """
    import glob

    def py_files_in(directory):
        """Recursively grab python files in a given directory"""
        for f in glob.iglob(
            directory + os.sep + "**" + os.sep + "*.py", recursive=True
        ):
            if os.path.isfile(f) and not f.endswith("__init__.py"):
                yield f

    def py_modules_in(directory, module_root_dir):
        """
        Recursively grab python module names in a given directory relative to a
        given module root:
        """
        for f in py_files_in(directory):
            yield os.path.relpath(f, module_root_dir)[:-3].replace(os.sep, "."), f

    def get_immediate_subdirectories(a_dir):
        """List immediate subdirs of a directory."""
        def listdir_nohidden(path):
            for f in os.listdir(path):
                if not f.startswith("."):
                    yield f

        if not os.path.exists(a_dir) or not os.path.isdir(a_dir):
            return []
        else:
            return [
                name
                for name in listdir_nohidden(a_dir)
                if os.path.isdir(os.path.join(a_dir, name))
            ]

    def is_matching_dir(directory):
        """Function that returns True or False if a directory matches the expected name"""
        return any([directory.endswith(name) for name in dir_names])

    # Get the python path:
    py_path = list(filter(bool, os.environ["PYTHONPATH"].split(os.pathsep)))

    # Go through all paths and see if any could contain generators:
    modules = []  # list of tuples (module_name, filename)
    for directory in py_path:
        # Make sure directory does not end in "/"
        if directory.endswith(os.sep):
            directory = directory[:-1]

        # If a sub directory ends with the word generators or generator import
        # all contained python modules recursively:
        subdirs = get_immediate_subdirectories(directory)
        matching_subdirs = list(filter(is_matching_dir, subdirs))
        for subdir in matching_subdirs:
            modules.extend(
                list(py_modules_in(os.path.join(directory, subdir), directory))
            )

    # sys.stderr.write("Discovered modules for " + str(dir_names) + ": " + str(modules) + "\n")
    return modules


def get_all_subclasses_of(cls):
    """
    Find all subclasses of a given class, recursively. This function
    will only find subclasses that are currently imported into the
    global namespace. So it is important to "import" all appropriate
    modules before using this function to search for subclasses.
    """
    return cls.__subclasses__() + [
        g for s in cls.__subclasses__() for g in get_all_subclasses_of(s)
    ]


def import_modules(modules):
    """
    Given a list of modules names, import them. These
    modules must already be in the PYTHONPATH
    """
    # http://ballingt.com/import-invalidate-caches/
    importlib.invalidate_caches()
    for module in modules:
        importlib.import_module(module)


def import_module_from_filename(py_file, module_name=None):
    """
    Import a python module via its filename path.
    This is slightly faster than the import_modules function above
    if you already know the filename path and module name.
    See: https://docs.python.org/3/library/importlib.html#importing-a-source-file-directly
    """
    if not module_name:
        module_name = os.path.splitext(os.path.basename(py_file))[0]

    if module_name in sys.modules:
        return sys.modules[module_name]
    else:
        spec = importlib.util.spec_from_file_location(module_name, py_file)
        mod = importlib.util.module_from_spec(spec)
        # http://ballingt.com/import-invalidate-caches/
        importlib.invalidate_caches()
        spec.loader.exec_module(mod)
        sys.modules[module_name] = mod
        return mod
