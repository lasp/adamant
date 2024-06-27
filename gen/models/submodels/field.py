from models.submodels.variable import variable
from models.exceptions import ModelException, throw_exception_with_lineno
from util import ada
import re


def parse_format(format_string):
    """Returns size, unit_type, unit_size, and count for a format string"""
    def extractNumber(string):
        return int("".join(filter(lambda x: x.isdigit(), string)))

    split_repr = format_string.split("x")
    if len(split_repr) > 1:
        return (
            split_repr[0].strip().upper(),
            extractNumber(split_repr[0]),
            extractNumber(split_repr[1]),
        )
    return format_string.upper(), extractNumber(format_string), None


class format(object):
    def __init__(self, format_string):
        # Validate the format string:
        regex = "^[uUiIfFeE][0-9]+(x[0-9]+)?$"
        if not re.match(regex, format_string):
            raise ModelException(
                "Found invalid format string '"
                + format_string
                + "'. Format strings must be of the form letter followed by number, adhering to the regex '"
                + regex
                + "'. Ie. U32, F16x5, I1, E8."
            )

        self.string = format_string.capitalize()
        # Parse the format string into:
        # The type: ie. U8, F32, etc.
        # The unit size (in bits) of the type: ie. 8, 32, etc.
        # The length (if an array) ie. for U8x5, the length would be 5
        self.type, self.unit_size, self.length = parse_format(format_string)

    def total_size_in_bits(self):
        if self.length:
            return self.unit_size * self.length
        return self.unit_size

    def __repr__(self):
        return self.type + ("x" + str(self.length) if self.length else "")

    def __str__(self):
        return self.__repr__()

    def get_total_bitarray_string(self):
        lookup = {"F": "floatbe", "I": "int", "U": "uint", "E": "uint"}
        return lookup[self.type[0].upper()] + ":" + str(self.total_size_in_bits())

    def get_bitarray_string(self):
        lookup = {"F": "floatbe", "I": "int", "U": "uint", "E": "uint"}
        if self.length:
            return (
                str(self.length)
                + "*"
                + lookup[self.type[0].upper()]
                + ":"
                + str(self.unit_size)
            )
        return lookup[self.type[0].upper()] + ":" + str(self.unit_size)

    def get_element_bitarray_string(self):
        lookup = {"F": "floatbe", "I": "int", "U": "uint", "E": "uint"}
        return lookup[self.type[0].upper()] + ":" + str(self.unit_size)

    def get_python_type_string(self):
        lookup = {"F": "float", "I": "int", "U": "int", "E": "int"}
        return lookup[self.type[0].upper()]

    def get_matlab_type_string(self):
        type_letter = self.type[0].upper()
        if type_letter == "F":
            # Handle float:
            if self.unit_size == 32:
                return "single"
            elif self.unit_size == 64:
                return "double"
            else:
                raise ModelException(
                    "Unsupported float size: "
                    + str(self.unit_size)
                    + ". Expected either 32 or 64."
                )
        else:
            # Handle integer:
            lookup = {"I": "int", "U": "uint", "E": "int"}
            rounded_length = 0
            if self.unit_size <= 8:
                rounded_length = 8
            elif self.unit_size <= 16:
                rounded_length = 16
            elif self.unit_size <= 32:
                rounded_length = 32
            elif self.unit_size <= 64:
                rounded_length = 64
            else:
                raise ModelException(
                    "Unsupported integer size: "
                    + str(self.unit_size)
                    + ". Expected less than or equal to 64."
                )
            return lookup[type_letter] + str(rounded_length)

    def get_printf_string(self):
        lookup = {"F": "f", "I": "d", "U": "u", "E": "d"}
        return lookup[self.type[0].upper()]

    def get_seq_type_string(self):
        lookup = {"F": "F", "I": "I", "U": "U", "E": "D"}
        return lookup[self.type[0].upper()]


class field(variable):
    def __init__(
        self,
        name,
        type,
        start_bit,
        start_field_number,
        format_string=None,
        description=None,
        value=None,
        default_value=None,
        variable_length=None,
        variable_length_offset=None,
        byte_image=False,
        skip_validation=False,
    ):
        self.format = None
        self.size = None  # in bits
        self.start_bit = start_bit
        self.end_bit = None
        self.start_field_number = start_field_number
        self.end_field_number = start_field_number
        self.byte_image = byte_image
        self.skip_validation = skip_validation

        # Call base class:
        # A packed record field could actually be a complex type like an packed
        # record itself, of an array. If that is the case, there will be a model
        # for this type. Let's make sure we tell the variable class to load a type
        # model if possible.
        super(field, self).__init__(
            name=name,
            type=type,
            description=description,
            value=value,
            default_value=default_value,
        )

        # Note: field inherits from variable, because it essentially is a variable.
        # However, the Ada standard capitalizes the name of record fields. So lets
        # do that here:
        self.name = self.name[0].upper() + self.name[1:]

        # Set variable length entities:
        self.variable_length = None
        self.variable_length_field = None
        self.variable_length_record = None
        if variable_length:
            self.variable_length = ".".join(
                [ada.formatType(name) for name in variable_length.split(".")]
            )
        self.variable_length_offset = 0
        if variable_length_offset:
            if not self.variable_length:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' specifies a 'variable_length_offset' of '"
                    + str(variable_length_offset)
                    + "' but does not specify a 'variable_length' field. A 'variable_length' "
                    + "field MUST be specified in order to specify a 'variable_length_offset'."
                )
            self.variable_length_offset = variable_length_offset

        # Parse format:
        if format_string:
            try:
                self.format = format(format_string)
            except ModelException as e:
                raise ModelException(
                    "Encountered error while parsing 'format' of field '"
                    + self.name
                    + "' of type '"
                    + self.type
                    + "': "
                    + str(e)
                )

        # This field contains a type model, lets grab some useful info:
        if self.is_packed_type:
            # Make sure type model exists:
            if not self.type_model:
                raise ModelException(
                    "Could not find type model '"
                    + self.type
                    + "' for field '"
                    + self.name
                    + "'."
                )

            # If this type includes a format field, that is a user error, since complex
            # types already have a pre-defined size:
            if self.format:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' of type '"
                    + self.type
                    + "' is a complex type using the model: "
                    + str(self.type_model.full_filename if self.type_model else None)
                    + ". Specifying 'format' for a field using a packed type is not permissible "
                    + "since its size is already defined."
                )

            # If the field is variable length, make sure that the type model is a packed array and not a packed record:
            if variable_length and not hasattr(self.type_model, "length"):
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' is of variable length, but is not of packed array type. Variable length "
                    + "fields either need to be packed arrays or arrays of primitive types."
                )

            # If the field is a variable length packed array, make sure the elements of that array are byte aligned.
            if (
                variable_length
                and hasattr(self.type_model, "length")
                and self.type_model.element.format
                and self.type_model.element.format.unit_size % 8 != 0
            ):
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' is of a variable length packed array type with elements that are NOT byte aligned. "
                    + "Elements within a variable length packed array MUST BE byte aligned."
                )

            # Fill in sizes from model:
            self.size = self.type_model.size
            self.end_field_number = (
                self.start_field_number + self.type_model.num_fields - 1
            )
        else:
            # If this type does NOT include a format field, that is a user error:
            if not self.format:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' of type '"
                    + self.type
                    + "' is NOT a packed type. You should specify a 'format' for this type so that it can be "
                    + "sized correctly."
                )

            # If the field is variable length, make sure there is a length included in the format:
            if variable_length and not self.format.length:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' is of variable length, but is not declared as an arrayed type. You should specify a max "
                    + "array length in the field's 'format'. ie. "
                    + self.format.type
                    + "x20"
                )

            # If the field is variable length, make sure it is byte aligned:
            if variable_length and self.format.unit_size % 8 != 0:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' is of a variable length with array elements that are NOT byte aligned. Elements within a "
                    + "variable length array MUST BE byte aligned."
                )

            self.size = self.format.total_size_in_bits()
            if self.format.unit_size > 64:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' of type '"
                    + self.type
                    + "' cannot have a format unit size over 64-bits. Consider using an array."
                )

        self.end_bit = self.start_bit + self.size - 1

    def resolve_variable_length_field(self, record_obj):
        """
        Verify that the variable length field exists in the record itself,
        and set up the field so that it can easily access the length field:
        """
        saved_record_obj = None
        if self.variable_length:
            split_field_name = self.variable_length.split(".")
            for name in split_field_name:
                if not record_obj or name not in record_obj.fields:
                    raise ModelException(
                        "Field '"
                        + self.name
                        + "' specifies length field name '"
                        + self.variable_length
                        + "' which does not exist before field '"
                        + self.name
                        + "' in the record."
                    )
                field_obj = record_obj.fields[name]
                if field_obj.is_packed_type:
                    record_obj = field_obj.type_model
                    saved_record_obj = record_obj
                else:
                    record_obj = None
            if field_obj.is_packed_type:
                raise ModelException(
                    "Field '"
                    + self.name
                    + "' specifies length field name '"
                    + self.variable_length
                    + "' which is a packed type. Variable length fields must be of primitive type."
                )
            self.variable_length_field = field_obj
            self.variable_length_record = saved_record_obj

    @classmethod
    @throw_exception_with_lineno
    def from_field_data(cls, start_bit, field_number, field_data):
        name = field_data["name"]
        type = field_data["type"]
        format_string = None
        if "format" in field_data:
            format_string = field_data["format"]
        description = None
        if "description" in field_data:
            description = field_data["description"]
        default_value = None
        if "default" in field_data:
            default_value = field_data["default"]
        variable_length = None
        if "variable_length" in field_data:
            variable_length = field_data["variable_length"]
        variable_length_offset = None
        if "variable_length_offset" in field_data:
            variable_length_offset = field_data["variable_length_offset"]
        byte_image = False
        if "byte_image" in field_data:
            byte_image = bool(field_data["byte_image"])
        skip_validation = False
        if "skip_validation" in field_data:
            skip_validation = bool(field_data["skip_validation"])
        return cls(
            name=name,
            type=type,
            start_bit=start_bit,
            start_field_number=field_number,
            format_string=format_string,
            description=description,
            default_value=default_value,
            variable_length=variable_length,
            variable_length_offset=variable_length_offset,
            byte_image=byte_image,
            skip_validation=skip_validation,
        )

    def is_always_valid(self):
        """
        Returns true if the type is always valid, meaning running
        'Valid on the type will ALWAYS produce True. In other words, there
        is no bit representation of the type that could cause a constraint
        error when a range check is performed.

        To determine this we need to compare the type's type_range against
        the its bit layout (ie. format) to ensure that the maximum representable
        values in the format is also the maximum representable values in the
        type itself.

        Note: The caller of this must have already loaded the outside packed type
        (packed array or record) type_ranges or the function call will fail.
        """
        # If this field is a packed type then just call its is_always_valid method:
        if self.is_packed_type:
            return self.type_model.is_always_valid()
        # Type with skip_validation is assumed to be is_always_valid by user decree.
        elif self.skip_validation:
            return True
        # If the type has a length in the format there is no easy way to generate the
        # array component type ranges in python. Maybe we can fix this later, but for
        # not we take the conservative assumption that it is not always valid. This
        # can be worked around by the user by either skipping validation on the
        # field with a length, or by using a packed array, which we can check correctly.
        elif self.format.length:
            return False
        # This is not a packed type, determine if type is valid based on size in bits.
        else:
            # The caller to this function must have already loaded the type ranges for this type, otherwise
            # the ranges won't be available for the comparison below.
            assert self.type_ranges_loaded, (
                "This should be True for '"
                + str(self.name)
                + " : "
                + str(self.type)
                + "' based on assumptions of this method."
            )

            # Get the number of possible representable values based on size in bits:
            num_possible_values = 2**self.size

            # Get the number of possible values in the Ada type based on range:
            if self.literals is not None:
                num_type_values = len(self.literals)
            else:
                num_type_values = self.range_max - self.range_min + 1

            # Compare and make sure they are equal:
            if num_type_values == num_possible_values:
                return True
            else:
                return False
