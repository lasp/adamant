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
    # it is experimental and unused, and is disabled by default. It is not
    # stable, it is not safe to share between redo sessions running at the
    # same time, and a variant of a model stored in it, a shallow load for
    # example, is never checked against its files.
    if "ENABLE_PERSISTENT_MODEL_CACHE" in environ:
        # Model cache is persistent over calls to redo (faster):
        cache_file = environ["ADAMANT_TMP_DIR"] + sep + "model_cache.db"
        return cache_file
    else:
        # Model cache is recreated for each call to redo (safer):
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
        Store cached version of model object along with timestamp of save and session ID
        for save. A variant, if given, stores the object apart from the model's main
        entry and from its other variants.
        """
        model_key = _model_key(model_file, variant)
        self.store(model_key, model_object)
        self.store(model_key + "_time@st@@", curr_time())
        self.store(model_key + "_sess@id@@", environ["ADAMANT_SESSION_ID"])
        if model_object.submodels is not None:
            submodel_paths = model_loader._get_model_file_paths(model_object.model_name)
            self.store(model_key + "_submod@paths@@", submodel_paths)

    def mark_cached_model_up_to_date_for_session(self, model_file, variant=""):
        """
        Update the session ID for a model. We use this to indicate that a model has been
        fully validated (ie. not outdated) for this redo session.
        """
        self.store(_model_key(model_file, variant) + "_sess@id@@", environ["ADAMANT_SESSION_ID"])

    def get_model(self, model_file, variant=""):
        """Getters for cached model, session id of save, and time of save"""
        return self.try_fetch(_model_key(model_file, variant))

    def get_model_session_id(self, model_file, variant=""):
        return self.try_fetch(_model_key(model_file, variant) + "_sess@id@@")

    def get_model_time_stamp(self, model_file, variant=""):
        return self.try_fetch(_model_key(model_file, variant) + "_time@st@@")

    def get_model_submodels(self, model_file, variant=""):
        return self.try_fetch(_model_key(model_file, variant) + "_submod@paths@@")


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
