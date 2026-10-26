from models.type import type
from models.exceptions import ModelException
import os
from collections import OrderedDict


class array(type):
    """
    This is the object model for a packed array. It extracts data from a
    input file and stores the data as object member variables.
    """
    shareable = True

    def __init__(self, filename):
        """
        Initialize the packed array object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        super(array, self).__init__(filename, os.environ["SCHEMAPATH"] + "/array.yaml")

    def _load(self):
        """Load record specific data structures with information from YAML file."""
        # Initialize object members:
        self.length = None

        # Define the endiannesses available for this packed record. Depending
        # on the packed record definition, the endiannesses supported are:
        #
        #   big    - The single packed type T is big endian
        #   little - The single packed type T_Le is little endian
        #   either - Two packed types exists T, big endian, and T_Le, little endian
        #   mixed  - The single packed type has both little and big endian parts
        #             ^ This one is not yet supported, but could be in the future.
        #
        self.endianness = "either"  # This is the default
        self.nested = False

        # Call base class load:
        super(array, self)._load()

        # Extract length and set appropriate fields:
        self.length = self.data["length"]

        # If element is arrayed then the array components must be <= 8 bits otherwise
        # endianness cannot be guaranteed. In this case, the user should be using a
        # packed array to declare the field type instead.
        if (
            self.element.format
            and self.element.format.length
            and self.element.format.length > 1
            and self.element.format.unit_size > 8
        ):
            raise ModelException(
                "Array '"
                + self.name
                + '" cannot specify component "'
                + self.element.name
                + "' of type '"
                + self.element.type
                + "' and format '"
                + str(self.element.format)
                + "'. Components of array type must be <=8 bits in size to guarantee endianness."
                + " Use a packed array to define arrays with components >8 bits in size."
            )

        # Calculate the number of fields in the array:
        if self.element.is_packed_type:
            self.num_fields = self.element.type_model.num_fields * self.length
            self.nested = True

            if (
                self.element.type.endswith(".T")
                or self.element.type.endswith(".Volatile_T")
                or self.element.type.endswith(".Atomic_T")
                or self.element.type.endswith(".Register_T")
            ):
                self.endianness = "big"
            elif (
                self.element.type.endswith(".T_Le")
                or self.element.type.endswith(".Volatile_T_Le")
                or self.element.type.endswith(".Atomic_T_Le")
                or self.element.type.endswith(".Register_T_Le")
            ):
                self.endianness = "little"
            else:
                raise ModelException(
                    "Array '"
                    + self.name
                    + '" cannot specify element "'
                    + self.element.name
                    + "' of type '"
                    + self.element.type
                    + "'. Nested packed types must either be '.*T' or '.*T_Le' types."
                )
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

        # Compute a static literal that is an upper bound on the
        # alignment GNAT will pick for this packed array's T (and
        # T_Le) types. The validation autocode uses this literal in
        # `with Alignment => ...` aspects on local Byte_Array copies
        # that are then overlaid by `R : T`. The safety condition for
        # the overlay (RM 13.3(13/3)) is met when `our_literal >=
        # T'Alignment` and `our_literal mod T'Alignment = 0`. Both
        # follow from "our literal is a power of 2 >= T'Alignment"
        # because GNAT-RM 10.1 guarantees T'Alignment is also a power
        # of 2 (any pow2 >= a smaller pow2 is a multiple of it).
        #
        # Records always have Alignment => 1 by GNAT-RM 13.4. For
        # everything else we cannot match both targets exactly with
        # one formula, so we pick a literal that is an upper
        # bound on both. Specifically:
        #   * byte-aligned: bytes_per_elem & -bytes_per_elem
        #   * sub-byte: next_pow2 of total array byte size
        # Over-aligning is harmless beyond a small stack-space cost
        # for the local Byte_Array copy. Under-aligning would be
        # erroneous. The pragma at every overlay site catches any
        # under-alignment at compile time.
        #
        # The `n & -n` idiom isolates the lowest set bit, which for
        # any positive integer is the greatest power of 2 dividing
        # it.
        if self.element.is_packed_type and not isinstance(
            self.element.type_model, array
        ):
            # Packed-record element: T'Alignment = 1, also enforced
            # by the parallel pragma Compile_Time_Error in the
            # record-validation template.
            self.required_alignment = 1
        elif self.element.size % 8 == 0:
            # Byte-aligned element: primitive (matches T'Alignment
            # exactly) or nested packed array (an upper bound; the
            # pragma at the overlay site enforces correctness).
            bytes_per_elem = self.element.size // 8
            self.required_alignment = bytes_per_elem & -bytes_per_elem
        else:
            # Sub-byte primitive (e.g. U10): GNAT picks next_pow2 of
            # the array's total byte size. Round up to next power of
            # 2: 1<<((x-1).bit_length()) for x>=1.
            total_bytes = self.size // 8
            self.required_alignment = 1 << max(0, (total_bytes - 1).bit_length())

        # Sanity-check: Required_alignment must be a power
        # of 2. Per the GNAT Reference Manual section 10.1 (Alignment
        # Clauses): "GNAT requires that all alignment clauses specify
        # 0 or a power of 2, and all default alignments are always a
        # power of 2." So T'Alignment is also pow2, and together with
        # this assertion that means `our_literal >= T'Alignment` (the
        # template's Compile_Time_Error) implies `our_literal mod
        # T'Alignment = 0` -- any pow2 >= a smaller pow2 is a
        # multiple of it.
        assert (
            self.required_alignment > 0
            and (self.required_alignment & (self.required_alignment - 1)) == 0
        ), (
            "Required_alignment="
            + str(self.required_alignment)
            + " for array '"
            + self.name
            + "' is not a power of 2, but must be per GNAT RM 10.1"
        )

        # Make sure the array does not contain a variable length type:
        if self.variable_length:
            raise ModelException(
                "Packed array '"
                + self.name
                + "' cannot specify variable length element. Packed arrays must have "
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

    def flatten(self):
        """Returns a flat ordered list of field objects that make up this array."""
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

    def is_always_valid(self):
        """
        Returns true if the packed type is always valid, meaning running
        'Valid on the element type will ALWAYS produce True. In other words, there
        is no bit representation of the type that could cause a constraint
        error when a range check is performed.

        To determine this we need to compare the type's type_range against
        the its bit layout (ie. format) to ensure that the maximum representable
        values in the format is also the maximum representable values in the
        type itself.
        """
        # First we need to load the type ranges for this type:
        self.load_type_ranges()
        return self.element.is_always_valid()
