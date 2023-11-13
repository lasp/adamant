from database.database import database
from database.database import DATABASE_MODE
from database import util


# This database is a catch all for random key value pairs.
# An example is shown below.
#
# Key:         Value:
# "some key" : [some, {"value" : "of"}, any, <type>]
#
class utility_database(database):
    # Initialize the database:
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        super(utility_database, self).__init__(util.get_database_file("utility"), mode)

    # Get a list of all the model files found in the
    # build path.
    def get_all_models(self):
        return self.fetch("model_files")

    # Get the build path object, which is a dictionary
    # that maps all the directories in the path to
    # a list of all the files in each directory.
    def get_build_path(self):
        return self.fetch("build_path")

    # Get a list of all the directories in the source
    # build path, usually used for compiling
    def get_source_build_path(self):
        return self.fetch("source_build_path")

    # Get a list of all the directories in the object
    # build path, usually used for linking.
    def get_object_build_path(self):
        return self.fetch("object_build_path")
