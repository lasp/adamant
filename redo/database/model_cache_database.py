from database.database import database
from database.database import DATABASE_MODE
from time import time as curr_time
from os import environ, sep
from util import model_loader
from database.util import get_database_file

# The purpose of the model cache is to save off YAML file model load
# data structures after they have been read from a file, validated, and
# consumed into objects for use with templates. This is a time-consuming
# process, and many models are used more than once during a redo build.
# So saving off these loaded objects into a database for quick load later
# saves us a lot of time during autocoding.


def get_model_cache_filename():
    # Decide whether or not to use a persistent or non-persistent model cache.
    # A persistent model cache that lasts over calls to redo is faster, but
    # is not stable and potentially buggy. persistent model cache is disabled
    # by default.
    if "ENABLE_PERSISTENT_MODEL_CACHE" in environ:
        # Model cache is persistent over calls to redo (faster):
        cache_file = environ["ADAMANT_TMP_DIR"] + sep + "model_cache.db"
        return cache_file
    else:
        # Model cache is recreated for each call to redo (safer):
        return get_database_file("model_cache")


class model_cache_database(database):
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(model_cache_database, self).__init__(get_model_cache_filename(), mode)

    def store_model(self, model_file, model_object):
        """
        Store cached version of model object along with timestamp of save and session ID
        for save.
        """
        self.store(model_file, model_object)
        self.store(model_file + "_time@st@@", curr_time())
        self.store(model_file + "_sess@id@@", environ["ADAMANT_SESSION_ID"])
        if model_object.submodels is not None:
            submodel_paths = model_loader._get_model_file_paths(model_object.model_name)
            self.store(model_file + "_submod@paths@@", submodel_paths)

    def mark_cached_model_up_to_date_for_session(self, model_file):
        """
        Update the session ID for a model. We use this to indicate that a model has been
        fully validated (ie. not outdated) for this redo session.
        """
        self.store(model_file + "_sess@id@@", environ["ADAMANT_SESSION_ID"])

    def get_model(self, model_file):
        """Getters for cached model, session id of save, and time of save"""
        return self.try_fetch(model_file)

    def get_model_session_id(self, model_file):
        return self.try_fetch(model_file + "_sess@id@@")

    def get_model_time_stamp(self, model_file):
        return self.try_fetch(model_file + "_time@st@@")

    def get_model_submodels(self, model_file):
        return self.try_fetch(model_file + "_submod@paths@@")


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
