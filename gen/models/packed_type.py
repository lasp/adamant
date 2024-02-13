from models.base import base
from util import ada
from util import redo_arg
from util import model_loader
from util import redo
import os
import abc


# This is the object model for a packed type. It is meant to be
# the base class for more specific packed types like: record, array
# and simple packed types (ie. type).
class packed_type(base):
    # Initialize the packed array object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename, template):
        # Load the object from the file:
        super(packed_type, self).__init__(filename, template)

    # Abstract method which does specific loading of the packed type.
    # The load() method below does the common loading for all packed types
    # and then calls this abstract method.
    @abc.abstractmethod
    def _load(self):
        pass

    # Get the model types, recursively:
    @abc.abstractmethod
    def get_all_types_recursive(self):
        pass

    # Get all type models, recursively:
    @abc.abstractmethod
    def get_all_type_models_recursive(self):
        pass

    # Set all the type ranges inside given a type ranges model.
    @abc.abstractmethod
    def set_type_ranges(self, type_ranges_model):
        pass

    # Load record specific data structures with information from YAML file.
    def load(self):
        # Initialize object members:
        self.includes = []
        self.description = None
        self.preamble = None
        self.name = None
        self.size = (
            0  # The maximum size of the type in the case of a variable sized type
        )
        self.num_fields = 0
        self.complex_types = None
        self.simple_types = None
        self.deps_list = []
        self.type_includes = None
        self.complex_type_includes = None
        self.variable_length = False
        self.type_ranges_loaded = False

        # Populate the object with the contents of the
        # file data:
        if "with" in self.data and self.data["with"]:
            self.includes = self.data["with"]
        for include in self.includes:
            include = ada.formatType(include)
        self.name = ada.formatType(self.model_name)
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        self._load()

    # Get model dependencies:
    def get_dependencies(self):
        return super().get_dependencies() + self.deps_list

    def load_type_ranges(self):
        if not self.type_ranges_loaded:
            # Build and load the type ranges model for this packed record.
            type_ranges_yaml = (
                redo_arg.get_src_dir(self.full_filename)
                + os.sep
                + "build"
                + os.sep
                + "yaml"
                + os.sep
                + self.model_name
                + ".type_ranges.yaml"
            )
            redo.redo_ifchange(type_ranges_yaml)
            type_ranges_model = model_loader.load_model(type_ranges_yaml)

            # Call the inherited abstract method.
            self.set_type_ranges(type_ranges_model)
        self.type_ranges_loaded = True
