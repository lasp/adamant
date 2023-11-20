from database.database import database
from database.database import DATABASE_MODE
from database import util

# The purpose of the model cache is to save off YAML file model load
# data structures after they have been read from a file, validated, and
# consumed into objects for use with templates. This is a time-consuming
# process, and many models are used more than once during a redo build.
# So saving off these loaded objects into a database for quick load later
# saves us a lot of time during autocoding.


def get_model_cache_filename():
    return util.get_database_file("model_cache")


class model_cache_database(database):
    # Initialize the database:
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        super(model_cache_database, self).__init__(get_model_cache_filename(), mode)

    def store_model(self, model_file, model_object):
        self.store(model_file, model_object)

    def get_model(self, model_file):
        return self.try_fetch(model_file)


# Create an empty model cache database file, if one does
# not already exist:
def touch_model_cache_database():
    import os.path

    filename = get_model_cache_filename()
    if not os.path.isfile(filename):
        with model_cache_database(mode=DATABASE_MODE.CREATE) as model_cache_db:
            # Get a dummy element to make sure database is created
            model_cache_db.get_model("dummy")
