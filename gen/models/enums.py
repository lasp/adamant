from models.exceptions import ModelException, throw_exception_with_lineno
from util import ada
from util import redo_arg
from collections import OrderedDict
import os
from models.base import base


class literal(object):
    def __init__(self, name, value=None, description=None):
        self.name = ada.formatType(name)
        self.value = value
        self.description = description

    @classmethod
    @throw_exception_with_lineno
    def from_literal_data(cls, literal_data):
        name = literal_data["name"]
        description = None
        if "description" in literal_data:
            description = literal_data["description"]
        value = None
        if "value" in literal_data:
            value = literal_data["value"]
        return cls(name=name, value=value, description=description)


class enum(object):
    def __init__(self, name, literals=[], description=None, suite=None):
        self.name = ada.formatType(name)
        self.literals = literals
        self.description = description
        self.suite = suite
        if self.suite:
            self.full_filename = suite.full_filename

        # Check literals names and make sure they are all unique:
        names = [lit.name for lit in self.literals]
        duplicate_names = set([x for x in names if names.count(x) > 1])
        if duplicate_names:
            raise ModelException(
                "Error encountered duplicate literal names: " + str(duplicate_names)
            )

        # Check values and make sure they are all unique:
        values = [lit.value for lit in self.literals if lit.value is not None]
        duplicate_values = set([x for x in values if values.count(x) > 1])
        if duplicate_values:
            raise ModelException(
                "Error encountered duplicate literal values: " + str(duplicate_values)
            )

        # Make sure all values are positive:
        for v in values:
            if v < 0:
                raise ModelException(
                    "Literal value '"
                    + str(v)
                    + "' not allowed. All literal values must be >= 0."
                )

        # Any values that are not yet set, lets fill in as the lowest number
        # possible.
        curr_val = 0
        for literal in self.literals:
            while literal.value is None:
                if curr_val not in values:
                    literal.value = curr_val
                    values.append(curr_val)
                curr_val += 1

        # Sort the literals:
        self.literals.sort(key=lambda x: x.value)

    @classmethod
    @throw_exception_with_lineno
    def from_enum_data(cls, enum_data, suite=None):
        name = enum_data["name"]
        description = None
        if "description" in enum_data:
            description = enum_data["description"]
        literals = []
        for literal_data in enum_data["literals"]:
            try:
                literals.append(literal.from_literal_data(literal_data))
            except ModelException as e:
                raise ModelException("Error encountered loading literal: " + str(e))
        return cls(name=name, literals=literals, description=description, suite=suite)

    def get_path_from(self, path_from):
        """Helper function to get file paths."""
        # Return the following:
        #  relative path to this model file from the provided path
        return os.path.relpath(self.full_filename, path_from)

    def get_src_dir_from(self, path_from):
        # Return the following:
        #  relative source path to this model dir from the provided path
        # ie. This is the same as get_dir_from except that if the result
        # is in a build directory, it returns the root directory where that
        # build directory lives.
        return redo_arg.get_src_dir(self.get_path_from(path_from))


class enums(base):
    """
    This is the object model for a suite of enumerations. It extracts data from an
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the packed type object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        super(enums, self).__init__(filename, os.environ["SCHEMAPATH"] + "/enums.yaml")

    def load(self):
        """Load record specific data structures with information from YAML file."""
        # Initialize object members:
        self.name = None
        self.description = None
        self.preamble = None
        self.enums = OrderedDict()
        self.includes = []

        # Populate the object with the contents of the
        # file data:
        self.name = ada.formatType(os.path.basename(self.model_name))
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        if "with" in self.data and self.data["with"]:
            self.includes = list(self.data["with"])

        # Load the enumerations:
        for enum_data in self.data["enums"]:
            try:
                e = enum.from_enum_data(enum_data, self)
                if e.name in self.enums:
                    raise ModelException(
                        "Enum list cannot contain duplicate enum type name '"
                        + str(e.name)
                        + "'."
                    )
                self.enums[e.name] = e
            except ModelException as e:
                raise ModelException("Error encountered loading enum: " + str(e))

    def get_enum_by_name(self, name):
        try:
            return self.enums[name]
        except KeyError:
            raise ModelException(
                "No enum '" + str(name) + "' exists in enum suite '" + self.name + "'."
            )
