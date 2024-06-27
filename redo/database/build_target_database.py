from database.database import database
from database.database import DATABASE_MODE
from database import util
from util import meta

# This database is responsible for storing "build targets" which are
# objects inheriting from build_target_base which provide information
# for compile source code to object files. The database maps the
# target name to an instance of the build target object and includes
# a second mapping useful for importing the module if it is not already
# imported. An example is shown below:
#
# Key:    Value:
# Linux : <python object instance for Linux build target>
# Linux_ImPortt* : (targets.linux, /path/to/linux.py)
#


class build_target_database(database):
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(build_target_database, self).__init__(
            util.get_database_file("build_target"), mode
        )

    def insert_build_target_instance(self, target_name, module_name, filename):
        self.store(target_name, (module_name, filename))

    def get_build_target_info(self, target_name):
        """
        For a given target name, like "Linux" provide information
        needed for creating an object of that target.
        """
        return self.fetch(target_name)

    def get_build_target_instance(self, target_name):
        """
        For a given target name, like "Linux" provide the build
        target object instance.
        """
        module_name, filename = self.get_build_target_info(target_name)
        # Import the module:
        mod = meta.import_module_from_filename(filename, module_name)
        # Get the class from within the module:
        target_class = getattr(mod, target_name)
        # Return the target instance and filename
        target_instance = target_class()
        return target_instance, filename

    def get_build_target_names(self):
        return list(self.keys())
