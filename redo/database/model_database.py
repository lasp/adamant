from database.database import database
from database.database import DATABASE_MODE
from database import util
from util import error
from util import ada
import os.path


def split_model_file_name(model_filename):
    """
    Split a model filename into its components and
    check to make sure that is complies with the adamant
    model file naming convention.
    """
    def err(string):
        error.error_abort(
            "The model file name '" + model_filename + "' is invalid. " + string
        )

    def regex_err(string):
        err(
            "All dot-separated fields in the model file name must match the regex \
                    '^[a-zA-Z][_?a-zA-Z0-9]*$'. Spaces are not allowed. The file name field '"
            + string
            + "' does not comply."
        )

    # Check the model filename:
    basename = os.path.basename(model_filename)
    splitB = basename.split(".")
    numFields = len(splitB)
    if numFields <= 2:
        err("A model type must be provided before the '.yaml' extension.")
    if numFields > 4:
        err("Too many dot-separated fields were found before the '.yaml' extension.")

    # Break up the model filename into its parts:
    assert (
        splitB[-1] == "yaml"
    ), "A non-yaml file should never be passed to this function."
    model_type = splitB[-2].lower()
    model_name = splitB[-3].lower()
    specific_name = None
    if numFields == 4:
        specific_name = splitB[-4].lower()

    # Make sure that all parts of the filename conform to the ada
    # variable naming conventions:
    if specific_name and not ada.is_valid_variable_name(specific_name):
        regex_err(specific_name)
    if not ada.is_valid_variable_name(model_name):
        regex_err(model_name)
    if not ada.is_valid_variable_name(model_type):
        regex_err(model_type)

    return model_name, model_type, specific_name


class model_database(database):
    """
    This database is responsible for storing model files found in
    the build path and showing how they relate together. The database
    maps model names to the model files that describe them.

    The layout is described below:

    Key:          Value:
    model_name    {"component" : [/path/to/model_name.component.yaml],
                   "ut" : [/path/to/tests1.model_name.ut.yaml, /path/to/tests2.model_name.ut.yaml],
                   "commands" : [/path/to/model_name.commands.yaml]}

    """
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(model_database, self).__init__(util.get_database_file("models"), mode)

    def insert_model(self, model_filename):
        """Insert a new model into the database."""
        def _add_to_list_entry(dic, key, value):
            try:
                dic[key].append(value)
            except Exception:
                dic[key] = [value]

        # Fetch the record for this model:
        model_name, model_type, specific_name = split_model_file_name(model_filename)
        record = self.try_fetch(model_name)

        # If record does not exist, create a blank one:
        if not record:
            record = {}

        # Add new information to the record:
        _add_to_list_entry(record, model_type, model_filename)

        # Store the updated record:
        self.store(model_name, record)

    def get_model_paths(self, model_name, model_type):
        """Given a model name and type return the full path."""
        record = self.try_fetch(model_name.lower())
        if record:
            if model_type in record:
                return record[model_type.lower()]
        return []

    def get_model_dict(self, model_name):
        """Given a model name return a dictionary mapping types to paths."""
        record = self.try_fetch(model_name.lower())
        if record:
            return record
        return {}

    def get_all_models(self):
        """Return a flat list of all (unique) yaml files stored in the database."""
        models = []
        for key in self.keys():
            record = self.fetch(key)
            for record_key, model_list in record.items():
                models.extend(model_list)
        return list(set(models))
