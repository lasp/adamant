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


def _get_model_file_paths(model_name, model_types=[]):
    """
    Given a model name and model type, return the full path to that model name. This function
    will thrown an error if more than one file is found containing the same model
    name and same model type:
    """
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


def get_model_file_paths(model_name, model_types=[]):
    """
    Given a model name and types, return all the full paths to models of that name and
    with matching types:
    """
    if model_types and isinstance(model_types, str):
        model_types = [model_types]
    return _get_model_file_paths(model_name, model_types)


def get_model_file_path(model_name, model_types=[]):
    """
    Given a model name, return the full path to that model name. This function
    will thrown an error if more than one file is found containing the same model
    name.
    """
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


# Shareable models already loaded by this process, by file and load
# arguments. A model class opts in with its shareable attribute; see base.
_shared_models = {}


def load_model(model_filename, *args, **kwargs):
    """
    Load a model from a given yaml file name. The extension on the filename is used
    to figure out which model class to use. The file extension must match the model
    class name.

    A shareable model (one its class marks as never altered after loading, such
    as a record or enumeration type) is loaded once per process and the same
    object returned for every later load of the same file with the same
    arguments. A load that asks to ignore the model cache always builds a fresh
    object, which then becomes the shared one, so the flag is honored rather
    than quietly served from memory.
    """
    model_class = _get_model_class(model_filename)
    if not getattr(model_class, "shareable", False):
        return model_class(model_filename, *args, **kwargs)
    key = (os.path.abspath(model_filename), args, tuple(sorted((k, v) for k, v in kwargs.items() if k != "ignore_cache")))
    if not kwargs.get("ignore_cache", False):
        try:
            return _shared_models[key]
        except KeyError:
            pass
        except TypeError:
            # Unhashable load arguments: load without sharing.
            return model_class(model_filename, *args, **kwargs)
    model = model_class(model_filename, *args, **kwargs)
    try:
        _shared_models[key] = model
    except TypeError:
        pass
    return model


def try_load_model_of_subclass(model_filename, parent_class, *args, **kwargs):
    model_class = _get_model_class(model_filename)
    if issubclass(model_class, parent_class):
        return model_class(model_filename, *args, **kwargs)
    return None


def try_load_model_by_name(model_name, model_types=[], *args, **kwargs):
    """
    Try to load a model with the given name and optional type(s). If no model is found
    than None is returned. This function will throw an error to the user if multiple
    files are found with the same model name and type.
    """
    # Get the model file paths from the database:
    model_file = get_model_file_path(model_name, model_types)

    # Load the model from the file if there only one was found:
    if model_file:
        return load_model(model_file, *args, **kwargs)
    return None


def try_load_models_by_name(model_name, model_types=[], *args, **kwargs):
    """
    Try to load all models with a given name and optional type(s). If no model is found
    than an empty list is returned. Otherwise a list of all loaded models is returned.
    """
    # Get the model file paths from the database:
    model_files = get_model_file_paths(model_name, model_types)

    # Return a list of loaded models:
    to_return = []
    for f in model_files:
        to_return.append(load_model(f, *args, **kwargs))
    return to_return


def load_project_configuration():
    """Helper to load the default adamant configuration file (*.configuration.yaml)"""
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
