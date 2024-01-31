from util import error as system_error
import database.model_database
import importlib.util
import os
from models.exceptions import ModelException

# This module provides useful functions for loading a YAML file
# into a model object. These functions are most commonly used
# to load one model object from another model that includes a
# model of that type, ie. a component model might load a command
# model if it contains commands. This loading of models within
# models allows us to write more expressive templates.


# Given a model name and model type, return the full path to that model name. This function
# will thrown an error if more than one file is found containing the same model
# name and same model type:
def _get_model_file_paths(model_name, model_types=[]):
    # Fetch the record from the model database:
    with database.model_database.model_database() as db:
        model_dict = db.get_model_dict(model_name)
    # Grab the model files out of the record:
    model_files = []
    if model_dict:
        if model_types:
            # Only get model files of the provided type:
            for model_type in model_types:
                model_type = model_type.lower()
                if model_type in model_dict:
                    model_files.extend(model_dict[model_type])
        else:
            # Grab all model files in record:
            model_files = [j for i in model_dict.values() for j in i]
    return model_files


# Given a model name and types, return all the full paths to models of that name and
# with matching types:
def get_model_file_paths(model_name, model_types=[]):
    if model_types and isinstance(model_types, str):
        model_types = [model_types]
    return _get_model_file_paths(model_name, model_types)


# Given a model name, return the full path to that model name. This function
# will thrown an error if more than one file is found containing the same model
# name.
def get_model_file_path(model_name, model_types=[]):
    model_files = get_model_file_paths(model_name=model_name, model_types=model_types)
    # Make sure only one model file was found:
    if model_files:
        if len(model_files) > 1:
            system_error.error_abort(
                'Trying to load model of name "'
                + model_name
                + '" and types: "'
                + str(model_types)
                + '" failed because more than one model of the name was found: '
                + str(model_files)
            )
        return model_files[0]
    return None


def _get_model_class(model_filename):
    (
        model_name,
        model_type,
        specific_name,
    ) = database.model_database.split_model_file_name(model_filename)

    # Import the model module:
    module_name = "models." + model_type
    try:
        # http://ballingt.com/import-invalidate-caches/
        importlib.invalidate_caches()
        module = importlib.import_module(module_name)
    except ImportError:
        system_error.error_abort(
            'Trying to load yaml model "'
            + model_filename
            + '" failed because no python module could be found called "'
            + module_name
            + '".'
        )

    # Return an instance of the module's class
    return getattr(module, model_type)


# Load a model from a given yaml file name. The extension on the filename is used
# to figure out which model class to use. The file extension must match the model
# class name.
def load_model(model_filename, *args, **kwargs):
    model_class = _get_model_class(model_filename)
    return model_class(model_filename, *args, **kwargs)


def try_load_model_of_subclass(model_filename, parent_class, *args, **kwargs):
    model_class = _get_model_class(model_filename)
    if issubclass(model_class, parent_class):
        return model_class(model_filename, *args, **kwargs)
    return None


# Try to load a model with the given name and optional type(s). If no model is found
# than None is returned. This function will throw an error to the user if multiple
# files are found with the same model name and type.
def try_load_model_by_name(model_name, model_types=[], *args, **kwargs):
    # Get the model file paths from the database:
    model_file = get_model_file_path(model_name, model_types)

    # Load the model from the file if there only one was found:
    if model_file:
        return load_model(model_file, *args, **kwargs)
    return None


# Try to load all models with a given name and optional type(s). If no model is found
# than an empty list is returned. Otherwise a list of all loaded models is returned.
def try_load_models_by_name(model_name, model_types=[], *args, **kwargs):
    # Get the model file paths from the database:
    model_files = get_model_file_paths(model_name, model_types)

    # Return a list of loaded models:
    to_return = []
    for f in model_files:
        to_return.append(load_model(f, *args, **kwargs))
    return to_return


# Helper to load the default adamant configuration file (*.configuration.yaml)
def load_project_configuration():
    # Load the configuration file from the environment variable:
    config_file = None
    if "ADAMANT_CONFIGURATION_YAML" in os.environ:
        config_file = os.environ["ADAMANT_CONFIGURATION_YAML"]
        if config_file:
            if not os.path.isfile(config_file):
                raise ModelException(
                    "Could not find Adamant configuration file in location: "
                    + config_file
                )
            config_file = os.path.realpath(config_file)

    # Set the default configuration file, if the environment variable is not set:
    if not config_file:
        base_dir = os.path.dirname(
            os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
        )
        config_file = base_dir + os.sep + "conf" + os.sep + "adamant.configuration.yaml"

    # Make sure the configuration file exists:
    if not os.path.isfile(config_file):
        raise ModelException(
            "Could not find Adamant configuration file in location: "
            + config_file
            + "\nMake sure a configuration file exists at this location or set the ADAMANT_CONFIGURATION_YAML"
            + " environment variable to use a different configuration file location."
        )

    # We have a valid configuration file, load it:
    from models import configuration

    return configuration.configuration(config_file)
