from database.database import database
from database.database import DATABASE_MODE
from database import util
from util import target
from util import redo_arg
from util import ada
from util import error
import os.path


class source_database(database):
    """
    This database is responsible for storing ada source files found in
    the build path. The database maps the ada package name to the source
    code used to build that package name, as well as any model files that
    are used to generate that source code. An example is shown below:

    Key:             Value:
    some.ada.package ([/path/to/some-ada-package.ads, \
    /path/to/some-ada-package.adb], \
    /path/to/some_ada_model.yaml)

    This database is useful for figuring out where ada source is located
    in the system given just a package name (which is all that is included
    in the "with" dependencies of an Ada source file).
    """
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(source_database, self).__init__(util.get_database_file("source"), mode)

    def insert_source(self, source_filename, model_filename=None):
        """
        Insert a source file into the database (and the associated model file, if
        there is one). Note that for a given package name (which is derived from the
        passed in source code) only a single .ads and a single .adb file may be added.
        If adding another file is attempted, the function will error and warn the user
        that every source in the build path must have a unique filename, and thus a
        unique package name.
        """
        # Error functions:
        def _duplicate_source_error(file1, file2):
            error.error_abort(
                ("All Ada source in the build path must have unique package and file names. "
                 "The following two source files conflict: [")
                + file1
                + ", "
                + file2
                + "]"
            )

        def _duplicate_model_error(file1, file2):
            error.error_abort(
                ("All models in the build path must produce unique package and file names. "
                 "The following two model files conflict: [")
                + file1
                + ", "
                + file2
                + "] because they both produce a file called '"
                + os.path.basename(source_filename)
                + "'"
            )

        # Get the Ada package name from the filename and try to
        # fetch a record if it exists.
        package_name = ada.file_name_to_package_name(source_filename).lower()
        record = self.try_fetch(package_name)

        # If the record already exists, we need to be very careful about adding another source
        # file. It must be the complement to the source file that already exists. For example,
        # if hello.adb exists, we can add hello.ads, but nothing else. If we find another piece of
        # source, then we potentially have two different packages named the same thing, and we
        # should warn the user.
        if record:
            # Make sure that the models don't conflict:
            existing_model = record[1]
            if existing_model and model_filename and existing_model != model_filename:
                _duplicate_model_error(existing_model, model_filename)

            # Check the source to make sure we are able to insert it.
            basename = os.path.basename(source_filename)
            existing_source = record[0]
            for source in existing_source:
                if source.endswith(basename):
                    # We have a specific conflict:
                    _duplicate_source_error(source_filename, source)

            # If this happens, we have some other weird conflict that we
            # definitely want to catch
            if len(existing_source) >= 2:
                _duplicate_source_error(source_filename, str(existing_source))
        else:
            record = [[], None]

        # OK, everything is good, let's go ahead and insert the source and model:
        record[0].append(source_filename)
        if model_filename:
            record[1] = model_filename

        # Insert record into database:
        self.store(package_name, record)

    def get_source(self, package_name):
        """Given a package name, return the associated source files."""
        return self.fetch(package_name.lower())[0]

    def try_get_source(self, package_name):
        """
        Given a package name, try to return the associated source files
        otherwise return an empty list.
        """
        try:
            return self.get_source(package_name)
        except KeyError:
            return []

    def get_sources(self, package_names):
        """Given a list of package names, return the associated source files."""
        sources = []
        if isinstance(package_names, str):
            package_names = [package_names]
        for name in package_names:
            sources.extend(self.get_source(name))
        return list(sources)

    def try_get_sources(self, package_names):
        """Given a list of package names, try to return the associated source files."""
        sources = []
        if isinstance(package_names, str):
            package_names = [package_names]
        for name in package_names:
            sources.extend(self.try_get_source(name))
        return list(sources)

    def get_objects(self, names, the_target=None):
        """Given a list of package names return the associated object files."""
        if the_target is None:
            the_target = target.get_default_target()
        objects = []
        for name in names:
            sources = self.try_get_source(name)
            if sources:
                source = next(
                    iter(sources)
                )  # just grab the first element from the set...
                if redo_arg.in_build_src_dir(source):
                    root_dir = os.path.dirname(os.path.dirname(os.path.dirname(source)))
                else:
                    root_dir = os.path.dirname(source)
                basename, ext = os.path.splitext(source)
                basename = os.path.basename(basename).lower()
                objects.append(
                    os.path.join(
                        root_dir,
                        "build"
                        + os.sep
                        + "obj"
                        + os.sep
                        + the_target
                        + os.sep
                        + basename
                        + ".o",
                    )
                )
        return list(objects)

    def get_model(self, package_name):
        """Given a package name, return the associated model file."""
        source_record = self.try_fetch(package_name.lower())
        if source_record:
            return source_record[1]
        return None
