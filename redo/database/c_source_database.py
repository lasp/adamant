from database.database import database
from database.database import DATABASE_MODE
from database import util
from util import redo_arg
from util import error
from util import target
import os.path


# This database is responsible for storing C/C++ source files found in
# the build path. The database maps the source filename with the extension
# removed to the source code location, as well as any model files that
# are used to generate that source code. An example is shown below:
#
# Key:       Value:
# c_filename ([/path/to/c_filename.c, \
#              /path/to/c_filename.h], \
#              /path/to/some_c_model.yaml)
#
# This database is useful for figuring out where c source is located
# in the system given just a base name (which may be all that is included
# in the "#include" dependencies of a C source file). Note that all
# entries in this database must be unique, meaning that C files in the
# build path of the same name, but in different directories are not
# allowed in Adamant, because of the interface with Ada, even though
# this pattern is allowed by the C language specification.
class c_source_database(database):
    # Initialize the database:
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        super(c_source_database, self).__init__(
            util.get_database_file("c_source"), mode
        )

    # Insert a source file into the database (and the associated model file, if
    # there is one). Note that for a given basename name (which is derived from the
    # passed in source code) only a single .c and a single .h file may be added.
    # If adding another file is attempted, the function will error and warn the user
    # that every source in the build path must have a unique filename. This is a
    # restriction of coding in C/C++ within Adamant do to the Ada interface.
    def insert_source(self, source_filename, model_filename=None):

        # Error functions:
        def _duplicate_source_error(file1, file2):
            error.error_abort(
                "All C/C++ source in the build path must have unique file names. The following two files conflict: ["
                + file1
                + ", "
                + file2
                + "]"
            )

        def _duplicate_model_error(file1, file2):
            error.error_abort(
                "All models in the build path must produce C/C++ file names. The following two files conflict: ["
                + file1
                + ", "
                + file2
                + "]"
            )

        # Get a basename from the filename and try to
        # fetch a record if it exists.
        basename = redo_arg.get_base_no_ext(source_filename)
        key = basename.lower()
        record = self.try_fetch(key)

        # If the record already exists, we need to be very careful about adding another source
        # file. It must be the compliment to the source file that already exists. For example,
        # if hello.c exists, we can add hello.h, but nothing else. If we find another piece of
        # source, then we potentially have two different packages named the same thing, and we
        # should warn the user.
        if record:
            # Make sure that the models don't conflict:
            existing_model = record[1]
            if existing_model and model_filename and existing_model != model_filename:
                _duplicate_model_error(existing_model, model_filename)

            # Check the source to make sure we are able to insert it.
            _, ext = os.path.splitext(source_filename)
            should_not_exist = [basename + ext]
            if ext.endswith("pp"):
                should_not_exist.append(basename + ext[:-2])
            else:
                should_not_exist.append(basename + ext + "pp")

            existing_source = record[0]
            for source in existing_source:
                source_base = os.path.basename(source)
                if source != source_filename and source_base in should_not_exist:
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
        self.store(key, record)

    # Given a basename, return the associated source files:
    def get_source(self, basename):
        return self.fetch(basename.lower())[0]

    # Given a basename, try to return the associated source files
    # otherwise return an empty list.
    def try_get_source(self, basename):
        try:
            return self.get_source(basename)
        except KeyError:
            return []

    # Given a list of basenames, return the associated source files:
    def get_sources(self, basenames):
        sources = []
        if isinstance(basenames, str):
            basenames = [basenames]
        for name in basenames:
            sources.extend(self.get_source(name))
        return list(sources)

    # Given a list of basenames, return the associated source files:
    def try_get_sources(self, basenames):
        sources = []
        if isinstance(basenames, str):
            basenames = [basenames]
        for name in basenames:
            sources.extend(self.try_get_source(name))
        return list(sources)

    # Given a list of package names return the associated object files:
    def get_objects(self, names, the_target=None):
        if the_target is None:
            the_target = target.get_default_target()
        objects = []
        for name in names:
            sources = self.try_get_source(name)
            # We assume common convention where C/C++ header files do not
            # get compiled into objects.
            if sources:
                sources = [filename for filename in sources if not filename.endswith((".h", ".hpp"))]
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

    # Given a basename, return the associated model file:
    def get_model(self, basename):
        source_record = self.try_fetch(basename)
        if source_record:
            return source_record[1]
        return None

    # Get all the object files that can be created by all the
    # source stored in the database:
    def get_all_objects(self, target):
        all_source = [a[0] for a in self.values()]
        flat_source_list = [item for sublist in all_source for item in sublist]
        all_objects = [
            redo_arg.src_file_to_obj_file(source, target) for source in flat_source_list
        ]
        return list(set(all_objects))

    def get_all_source_dirs(self):
        # Return all directories where source code can be located?
        # what about autocode?
        all_source = [a[0] for a in self.values()]
        flat_source_list = [item for sublist in all_source for item in sublist]
        all_dir_list = [os.path.dirname(source) for source in flat_source_list]
        return list(set(all_dir_list))
