from database.database import database
from database.database import DATABASE_MODE
from database import util


class utility_database(database):
    """
    This database is a catch all for random key value pairs.
    An example is shown below.

    Key:         Value:
    "some key" : [some, {"value" : "of"}, any, <type>]

    """
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(utility_database, self).__init__(util.get_database_file("utility"), mode)

    def get_all_models(self):
        """
        Get a list of all the model files found in the
        build path.
        """
        return self.fetch("model_files")

    def get_build_path(self):
        """
        Get the build path object, which is a dictionary
        that maps all the directories in the path to
        a list of all the files in each directory.
        """
        return self.fetch("build_path")

    def get_source_build_path(self):
        """
        Get a list of all the directories in the source
        build path, usually used for compiling
        """
        return self.fetch("source_build_path")

    def get_object_build_path(self):
        """
        Get a list of all the directories in the object
        build path, usually used for linking.
        """
        return self.fetch("object_build_path")
