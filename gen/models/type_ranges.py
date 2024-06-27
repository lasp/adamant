from models.exceptions import ModelException, throw_exception_with_lineno
from util import ada
from collections import OrderedDict
import os
from models.base import base


class type_literal(object):
    def __init__(self, name, value=None):
        self.name = ada.formatType(name)
        self.value = value

    @classmethod
    @throw_exception_with_lineno
    def from_literal_data(cls, literal_data):
        name = literal_data["name"]
        value = None
        if "value" in literal_data:
            value = literal_data["value"]
        return cls(name=name, value=value)


class type_enum(object):
    def __init__(self, name, literals=[]):
        self.name = ada.formatType(name)
        self.literals = literals

    @classmethod
    @throw_exception_with_lineno
    def from_enum_data(cls, enum_data):
        name = enum_data["name"]
        literals = []
        for literal_data in enum_data["literals"]:
            try:
                literals.append(type_literal.from_literal_data(literal_data))
            except ModelException as e:
                raise ModelException("Error encountered loading literal: " + str(e))
        return cls(name=name, literals=literals)


class type_number(object):
    def __init__(self, name, min=None, max=None):
        self.name = ada.formatType(name)
        self.min = min
        self.max = max

    @classmethod
    @throw_exception_with_lineno
    def from_number_data(cls, number_data):
        name = number_data["name"]
        min = number_data["min"]
        max = number_data["max"]
        return cls(name=name, min=min, max=max)


class type_ranges(base):
    """
    This is the object model for a set of type ranges enumerations. It extracts data from an
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the packed type object, ingest data, and check it by
        calling the base class init function.
        """
        try:
            # Load the object from the file:
            super(type_ranges, self).__init__(
                filename, os.environ["SCHEMAPATH"] + "/type_ranges.yaml"
            )
        except ModelException as e:
            raise ModelException(
                str(e)
                + "\nThe error above was encountered while loading Type Ranges file: "
                + filename
                + "\nThis often occurs when declaring an enumeration in a packed record or packed array "
                + "but forgetting to specify the format with an 'E', ie. E8 for an 8-bit enumeration. Or "
                + "forgetting to specify the format with an 'I' or 'U' for an integer, ie. U16 or I32. "
                + "Check your packed type yaml file."
            )

    def load(self):
        """Load record specific data structures with information from YAML file."""
        # Initialize object members:
        self.name = None
        self.types = OrderedDict()

        # Populate the object with the contents of the
        # file data:
        self.name = ada.formatType(os.path.basename(self.model_name))

        # Load the types:
        if "types" in self.data and self.data["types"]:
            for type_data in self.data["types"]:
                # This type is either an integer, in which case it must have both a min
                # and a max field or the type is an enumeration in which case it has a literals
                # field. Make sure it doesn't mix the two.
                if "max" in type_data:
                    if "min" not in type_data:
                        raise ModelException(
                            "Type ranges type '"
                            + str(type_data.name)
                            + " includes a 'max' but no 'min'."
                        )
                    if "literals" in type_data:
                        raise ModelException(
                            "Type ranges type '"
                            + str(type_data.name)
                            + " includes a 'max' and 'min', but also includes 'literals'. It cannot "
                            + "contain both max/min and literals."
                        )
                    # Load the number type:
                    try:
                        theType = type_number.from_number_data(type_data)
                    except ModelException as e:
                        raise ModelException(
                            "Error encountered loading number: " + str(e)
                        )
                elif "min" in type_data:
                    raise ModelException(
                        "Type ranges type '"
                        + str(type_data.name)
                        + " includes a 'min' but no 'max'."
                    )
                elif "literals" in type_data:
                    # Load the enum type:
                    try:
                        theType = type_enum.from_enum_data(type_data)
                    except ModelException as e:
                        raise ModelException(
                            "Error encountered loading enum: " + str(e)
                        )
                else:
                    raise ModelException(
                        "Type ranges type '"
                        + str(type_data.name)
                        + " must include a min/max or literals."
                    )

                # Save the type, make sure its unique.
                # if theType.name in self.types:
                #   raise ModelException("Cannot load duplicate type name: " + theType.name)
                self.types[theType.name] = theType

    def get_type_by_name(self, name):
        try:
            return self.types[name]
        except KeyError:
            raise ModelException(
                "No type '"
                + str(name)
                + "' exists in type ranges suite '"
                + self.name
                + "'."
            )
