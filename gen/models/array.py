from models.type import type
from models.exceptions import ModelException
import os
from collections import OrderedDict


# This is the object model for a packed array. It extracts data from a
# input file and stores the data as object member variables.
class array(type):
    # Initialize the packed array object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        super(array, self).__init__(filename, os.environ["SCHEMAPATH"] + "/array.yaml")

    # Load record specific data structures with information from YAML file.
    def _load(self):
        # Initialize object members:
        self.length = None

        # Call base class load:
        super(array, self)._load()

        # Extract length and set appropriate fields:
        self.length = self.data["length"]

        # Calculate the number of fields in the array:
        if self.element.is_packed_type:
            self.num_fields = self.element.type_model.num_fields * self.length
        else:
            self.num_fields = self.length

        # Calculate total size:
        self.size = self.element.size * self.length

        # Check size and make sure it is byte aligned.
        if self.size % 8 != 0:
            raise ModelException(
                "Packed array '"
                + self.name
                + "' must have size that is a multiple of 8-bits. Its current size is: "
                + str(self.size)
                + ". Packed arrays may have elements that are not byte aligned, but the "
                + "total packed array itself must be byte aligned."
            )

        # Make sure the array does not contain a variable length type:
        if self.variable_length:
            raise ModelException(
                "Packed array '"
                + self.name
                + "' cannot specify variable length element. Packed arrays must have " +
                + "statically sized elements."
            )

        # Create a fields attribute similar to records so the array can be
        # used in a similar way for autocoding.
        self.fields = OrderedDict()
        for idx in range(self.length):
            # 1/8/2020 - cannot do this deep copy, it takes up too much memory and time
            # for compilation on large arrays. Need to make sure we use the dictionary
            # key name instead of self.element.name when using this dictionary. The
            # flattened_names() function below uses this  pattern.
            # e = deepcopy(self.element)
            # e.name = self.element.name + "_" + str(idx)
            self.fields[self.element.name + "_" + str(idx)] = self.element

        # Is this type holding a volatile component?
        self.is_volatile_type = self.element.is_volatile_type
        self.is_register_type = self.element.is_register_type
        self.is_atomic_type = self.element.is_atomic_type

        # We will only create one of these types (the most specific). Save off the
        # correct descriptor.
        self.volatile_descriptor = None
        if self.is_volatile_type:
            self.volatile_descriptor = "Volatile"
        if self.is_register_type:
            self.volatile_descriptor = "Register"
        if self.is_atomic_type:
            self.volatile_descriptor = "Atomic"

    # Returns a flat ordered list of field objects that make up this array:
    def flatten(self):
        fields = []
        for a_field in self.fields.values():
            if a_field.is_packed_type:
                fields.extend(a_field.type_model.flatten())
            else:
                fields.append(a_field)
        return fields

    def flattened_names(self, prefix="", separator="."):
        names = []
        for name, a_field in self.fields.items():
            if a_field.is_packed_type:
                names.extend(
                    a_field.type_model.flattened_names(
                        (prefix if prefix else "") + name + separator
                    )
                )
            else:
                names.append(prefix + name)
        return names

    def flattened_descriptions(self, prefix="", separator="."):
        descriptions = []
        for name, a_field in self.fields.items():
            if a_field.is_packed_type:
                descriptions.extend(
                    a_field.type_model.flattened_descriptions(
                        (prefix if prefix else "")
                        + name
                        + " - "
                        + (a_field.description if a_field.description else "")
                        + separator,
                        separator,
                    )
                )
            else:
                descriptions.append(
                    prefix
                    + name
                    + " - "
                    + (a_field.description if a_field.description else "")
                )
        return descriptions

    def flatten_dict(self, prefix="", separator="."):
        items = self.flatten()
        names = self.flattened_names(prefix, separator)
        return OrderedDict(zip(names, items))

    def set_type_ranges(self, type_ranges_model):
        # For the element, if type is not a packed record, then set the
        # type range using the value in the type ranges model.
        if (
            not self.element.type_ranges_loaded
            and not self.element.is_packed_type
            and not self.element.format.length
            and not self.element.skip_validation
        ):
            type_range = type_ranges_model.get_type_by_name(self.element.type)
            # Assume this is a number with a standard min and max range:
            try:
                self.element.range_min = type_range.min
                self.element.range_max = type_range.max
            # OK this must be an enumeration instead:
            except AttributeError:
                self.element.literals = type_range.literals
            self.element.type_ranges_loaded = True

    # Returns true if the packed type is always valid, meaning running
    # 'Valid on the element type will ALWAYS produce True. In other words, there
    # is no bit representation of the type that could cause a constraint
    # error when a range check is performed.
    #
    # To determine this we need to compare the type's type_range against
    # the its bit layout (ie. format) to ensure that the maximum representable
    # values in the format is also the maximum representable values in the
    # type itself.
    def is_always_valid(self):
        # First we need to load the type ranges for this type:
        self.load_type_ranges()
        return self.element.is_always_valid()
