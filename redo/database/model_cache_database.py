from database.database import database
from database.database import DATABASE_MODE
from database.util import get_database_file

# The purpose of the model cache is to save off YAML file model load
# data structures after they have been read from a file, validated, and
# consumed into objects for use with templates. This is a time-consuming
# process, and many models are used more than once during a redo build.
# So saving off these loaded objects into a database for quick load later
# saves us a lot of time during autocoding.
#
# The model cache lasts for one redo session. It is created with the
# session and removed with it, so every model in it was built from its
# file during the session and is used without being checked against that
# file again.


def get_model_cache_filename():
    return get_database_file("model_cache")


def _model_key(model_file, variant):
    """
    The key a model is cached under. A variant names another form of the same
    model that is cached apart from it, a shallow load for example.
    """
    if variant:
        return model_file + "_" + variant + "@@"
    return model_file


class model_cache_database(database):
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(model_cache_database, self).__init__(get_model_cache_filename(), mode)

    def store_model(self, model_file, model_object, variant=""):
        """
        Store cached version of model object. A variant, if given, stores the object
        apart from the model's main entry and from its other variants.
        """
        self.store(_model_key(model_file, variant), model_object)

    def get_model(self, model_file, variant=""):
        """Getter for cached model"""
        return self.try_fetch(_model_key(model_file, variant))


def touch_model_cache_database():
    """
    Create an empty model cache database file, if one does
    not already exist:
    """
    import os.path

    filename = get_model_cache_filename()
    if not os.path.isfile(filename):
        with model_cache_database(mode=DATABASE_MODE.CREATE) as model_cache_db:
            # Get a dummy element to make sure database is created
            model_cache_db.get_model("dummy")
