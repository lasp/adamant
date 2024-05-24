from models.packed_type import packed_type
from models.exceptions import ModelException
from models.submodels.field import field
from collections import OrderedDict
import os


# This is the object model for a packed record. It extracts data from a
# input file and stores the data as object member variables.
class record(packed_type):
    # Initialize the events object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        super(record, self).__init__(
            filename, os.environ["SCHEMAPATH"] + "/record.yaml"
        )

    # Load record specific data structures with information from YAML file.
    def _load(self):
        # Initialize object members:
        self.fields = OrderedDict()
        self.variable_length_fields = OrderedDict()
        self.dynamically_sized_fields = OrderedDict()
        self.statically_sized_fields = OrderedDict()
        self.variable_length_sizing_fields = OrderedDict()

        #
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

        # Populate the object with the contents of the
        # file data:
        start_bit = 0
        self.variable_length = False
        count = 0
        for field_data in self.data["fields"]:
            self.num_fields += 1
            count += 1
            the_field = field.from_field_data(start_bit, self.num_fields, field_data)
            if the_field.name in self.fields:
                raise ModelException(
                    "Packed record '"
                    + self.name
                    + "' cannot contain duplicate field name: '"
                    + str(the_field.name)
                    + "'"
                )
            self.fields[the_field.name] = the_field
            start_bit = the_field.end_bit + 1
            self.num_fields = the_field.end_field_number

            # If field is arrayed then the array components must be <= 8 bits otherwise
            # endianness cannot be guaranteed. In this case, the user should be using a
            # packed array to declare the field type instead.
            if (
                the_field.format
                and the_field.format.length
                and the_field.format.length > 1
                and the_field.format.unit_size > 8
            ):
                raise ModelException(
                    "Record '"
                    + self.name
                    + '" cannot specify field "'
                    + the_field.name
                    + "' of type '"
                    + the_field.type
                    + "' and format '"
                    + str(the_field.format)
                    + "'. Array components must be <=8 bits in size to guarantee endianness."
                    + " Use a packed array to defined arrays with components >8 bits in size."
                )

            # Handle fields that are packed records or arrays themselves, ie. (nested packed records)
            if the_field.is_packed_type:
                self.nested = True

                # Check endianness. The endianness rules are as follows:
                #
                #   1. A simple packed record with no nesting of other packed records/arrays will support
                #      "either" endianness
                #   2. A nested packed record containing fields of .T types only support "big" endian
                #   3. A nested packed record containing fields of .T_Le types only supports "little" endian
                #   4. Packed records can not contain both little and big endian fields (for now) or unpacked
                #      fields.
                #
                if self.endianness == "either":
                    if (
                        the_field.type.endswith(".T")
                        or the_field.type.endswith(".Volatile_T")
                        or the_field.type.endswith(".Atomic_T")
                        or the_field.type.endswith(".Register_T")
                    ):
                        self.endianness = "big"
                    elif (
                        the_field.type.endswith(".T_Le")
                        or the_field.type.endswith(".Volatile_T_Le")
                        or the_field.type.endswith(".Atomic_T_Le")
                        or the_field.type.endswith(".Register_T_Le")
                    ):
                        self.endianness = "little"
                    else:
                        raise ModelException(
                            "Record '"
                            + self.name
                            + '" cannot specify field "'
                            + the_field.name
                            + "' of type '"
                            + the_field.type
                            + "'. Nested packed types must either be '.*T' or '.*T_Le' types."
                        )
                else:
                    if self.endianness == "big" and (
                        the_field.type.endswith(".T")
                        or the_field.type.endswith(".Volatile_T")
                        or the_field.type.endswith(".Atomic_T")
                        or the_field.type.endswith(".Register_T")
                    ):
                        pass  # all is good
                    elif self.endianness == "little" and (
                        the_field.type.endswith(".T_Le")
                        or the_field.type.endswith(".Volatile_T_Le")
                        or the_field.type.endswith(".Atomic_T_Le")
                        or the_field.type.endswith(".Register_T_Le")
                    ):
                        pass  # all is good
                    else:
                        raise ModelException(
                            "Record '"
                            + self.name
                            + '" cannot specify field "'
                            + the_field.name
                            + "' of type '"
                            + the_field.type
                            + "'. Nested packed types must ALL be either '.*T' or '.*T_Le' types. "
                            + "Mixed endianness is not currently supported for packed records."
                        )

            # Handle variable length fields:
            if the_field.variable_length:
                self.variable_length = True

                # Packed records in Adamant must be "overlayable" with a byte array. This property is only true
                # if the variable length field appears only at the end of a packed record. Check this:
                if count != len(self.data["fields"]):
                    raise ModelException(
                        "Record '"
                        + self.name
                        + '" must specify variable length field "'
                        + the_field.name
                        + "' as the last field in the record. Packed records may only have "
                        + "one variable length field, and it must be the last field in the record."
                    )

                # Packed records in Adamant must be "overlayable" with a byte array. This property is only true
                # if the variable length field is not made up of a variable length object:
                if the_field.is_packed_type and the_field.type_model.variable_length:
                    raise ModelException(
                        "Record '"
                        + self.name
                        + '" cannot specify variable length field "'
                        + the_field.name
                        + "' with a variable length type '"
                        + the_field.type
                        + "'. Variable length fields must contain statically sized element types."
                    )

                # Resolve the variable length field:
                try:
                    the_field.resolve_variable_length_field(self)
                except ModelException as e:
                    raise ModelException(
                        "Error encountered resolving the variable length field for field '"
                        + the_field.name
                        + "': "
                        + str(e)
                    )
                self.variable_length_fields[the_field.name] = the_field
                self.variable_length_sizing_fields[the_field.variable_length] = (
                    the_field.variable_length_field
                )

            # Handle a field that might have a variable length type:
            elif the_field.is_packed_type and the_field.type_model.variable_length:
                self.variable_length = True

                # Packed records in Adamant must be "overlayable" with a byte array. This property is only true
                # if the variable length field appears only at the end of a packed record. Check this:
                if count != len(self.data["fields"]):
                    raise ModelException(
                        "Record '"
                        + self.name
                        + '" must specify variable length field "'
                        + the_field.name
                        + "' as the last field in the record. Packed records may only have one "
                        + "variable length field, and it must be the last field in the record."
                    )

            # Set simple boolean to help with code generation:
            if the_field.is_packed_type and the_field.type_model.variable_length:
                self.dynamically_sized_fields[the_field.name] = the_field
            else:
                self.statically_sized_fields[the_field.name] = the_field

        # If record is variable length, then all of the fields MUST be byte aligned. Check this:
        if self.variable_length:
            for the_field in self.fields.values():
                if the_field.size % 8 != 0:
                    raise ModelException(
                        "Record '"
                        + self.name
                        + "' is a variable length record and cannot specify field '"
                        + the_field.name
                        + "' that has size '"
                        + str(the_field.size)
                        + "' which is not byte aligned. Variable length records may not contain fields "
                        + "that have sizes that are not a multiple of 8-bits. Consider putting non-byte "
                        + "aligned fields in their own record definition, ensuring that the total record "
                        + "is itself byte-aligned."
                    )

        # Calculate total size:
        self.size = start_bit

        # Calculate minimum size of packed record:
        self.min_size = self.size
        self.prefix_size = self.size
        self.total_variable_length_offset_bytes = 0

        # If the record is variable length the minimum size is a bit complicated to calculate. We need
        # search through the record until we find the variable length field, which is the last field in the
        # record, inside the lowest child packed type. Then we subtract its length from the maximum length.
        if self.variable_length and self.fields:
            last_field = list(self.fields.values())[-1]
            while last_field.is_packed_type and last_field.type_model.variable_length:
                last_field = list(last_field.type_model.fields.values())[-1]
            self.min_size = self.size - last_field.size
            self.prefix_size = self.size - list(self.fields.values())[-1].size
            if last_field.format is None:  # This is a packed array
                self.total_variable_length_offset_bytes = int(
                    int(
                        last_field.variable_length_offset
                        * last_field.type_model.element.size
                    )
                    / int(8)
                )
            else:
                self.total_variable_length_offset_bytes = int(
                    int(last_field.variable_length_offset * last_field.format.unit_size)
                    / int(8)
                )

        # Check size and make sure it is byte aligned.
        if self.size % 8 != 0:
            raise ModelException(
                "Packed record '"
                + self.name
                + "' must have size that is a multiple of 8-bits. Its current size is: "
                + str(self.size)
                + ". Packed records may have fields that are not byte aligned, but the "
                + "total packed record itself must be byte aligned. Please add bit padding "
                + "(reserved) fields if necessary."
            )

        # If a packed record has a volatile field, then all fields in the packed record must be
        # volatile. This is a strict requirement enforced by Adamant to ensure that records do
        # not mix volatile and non volatile record fields. There is no valid use case for this
        # pattern, and thus it is prevented. Volatile and non-volatile records should be grouped
        # separately.
        #
        # In addition, a packed record that holds volatile fields cannot specify important attributes
        # such as the scalar storage order and bit order due to the record being forced to be a
        # by-reference type as described in the RM. This means that a packed record holding volatile
        # components is little more than a shell wrapping the internal fields. Pragma pack can be
        # specified by setting the 'Size attribute, but little more can be specified. This means
        # the representation of the record is only determined by the representation of the internal
        # fields. To keep things clear, Adamant makes sure that any record holding a volatile component
        # this ONLY contains fields with volatile components (which by definition are also packed).
        # In this way the representation semantics are very clear when looking at the record, since
        # they are derived only from the representation of the internal fields.
        self.is_volatile_type = False
        self.is_register_type = False
        self.is_atomic_type = False
        for f in self.fields.values():
            if f.is_volatile_type:
                self.is_volatile_type = True
        if self.is_volatile_type:
            self.is_register_type = True
            self.is_atomic_type = True
            for f in self.fields.values():
                if not f.is_volatile_type:
                    raise ModelException(
                        "Packed record '"
                        + self.name
                        + "' contains a field that is (or contains) either a Volatile, Atomic, or "
                        + "Register type. A packed record that contains a field of this type must "
                        + "specify ALL fields as Volatile, Atomic, or Register type."
                    )
                if not f.is_register_type:
                    self.is_register_type = False
                if not f.is_atomic_type:
                    self.is_atomic_type = False

        # We will only create one of these types (the most specific). Save off the
        # correct descriptor.
        self.volatile_descriptor = None
        if self.is_volatile_type:
            self.volatile_descriptor = "Volatile"
        if self.is_register_type:
            self.volatile_descriptor = "Register"
        if self.is_atomic_type:
            self.volatile_descriptor = "Atomic"

        # Store useful lists:

        # Includes:
        self.includes = list(set(self.includes))
        # Store all types that have a model associated with them:
        complex_typed_fields = list(
            OrderedDict.fromkeys([f for f in self.fields.values() if f.type_model])
        )
        self.complex_types = list(
            OrderedDict.fromkeys([f.type for f in complex_typed_fields])
        )

        # Store all types that have a model associated with them:
        self.complex_type_models = list(
            OrderedDict.fromkeys(
                [f.type_model for f in self.fields.values() if f.is_packed_type]
            )
        )
        self.all_type_models = list(
            OrderedDict.fromkeys([f.type_model for f in complex_typed_fields])
        )

        # Get all unique enums models:
        seen_enums = OrderedDict()
        for obj in self.fields.values():
            if obj.is_enum and obj.type_model.name not in seen_enums:
                seen_enums[obj.type_model.name] = obj.type_model
        self.enum_models = list(seen_enums.values())

        # Store all types that do NOT have a model associated with them:
        self.simple_typed_fields = list(
            OrderedDict.fromkeys([f for f in self.fields.values() if not f.type_model])
        )
        self.simple_types = list(
            OrderedDict.fromkeys([f.type for f in self.simple_typed_fields])
        )
        self.simple_type_includes = list(
            OrderedDict.fromkeys(
                [f.type_package for f in self.simple_typed_fields if f.type_package]
            )
        )
        self.unpacked_typed_fields = list(
            OrderedDict.fromkeys(
                [f for f in self.fields.values() if not f.is_packed_type]
            )
        )
        self.unpacked_types = [f.type for f in self.unpacked_typed_fields]
        self.unpacked_type_includes = list(
            OrderedDict.fromkeys(
                [f.type_package for f in self.unpacked_typed_fields if f.type_package]
            )
        )

        # Store the filenames of all the type models used in this record.
        self.deps_list = list(
            OrderedDict.fromkeys(
                [f.type_model.full_filename for f in complex_typed_fields]
            )
        )

        # Store the includes necessary to include the field types:
        self.type_includes = list(
            OrderedDict.fromkeys(
                [f.type_package for f in self.fields.values() if f.type_package]
            )
        )

        # Create type uses list for assertion package. This is complicated, but needed.
        type_includes_no_var = []
        for f in self.fields.values():
            if f.type_model and f.is_packed_type:
                if not f.type_model.variable_length:
                    type_includes_no_var.append(f.type_package)
            elif f.type_model and not f.is_packed_type:
                type_includes_no_var.append(f.type_package + "." + f.type_model.name)
            elif f.type_package:
                type_includes_no_var.append(f.type_package)
        self.type_uses = list(OrderedDict.fromkeys(type_includes_no_var))

        # Store the includes for any complex types (those that have models).
        self.modeled_type_includes = list(
            OrderedDict.fromkeys([f.type_package for f in complex_typed_fields])
        )
        self.packed_type_includes = list(
            OrderedDict.fromkeys(
                [f.type_package for f in self.fields.values() if f.is_packed_type]
            )
        )

        # Store the includes necessary to include the field types:
        self.variable_length_type_includes = list(
            OrderedDict.fromkeys(
                [
                    f.variable_length_field.type_package
                    for f in self.variable_length_fields.values()
                    if f.variable_length_field.type_package
                ]
            )
        )
        self.variable_length_dynamically_sized_type_includes = list(
            OrderedDict.fromkeys(
                [
                    f.variable_length_field.type_package
                    for f in self.variable_length_fields.values()
                    if f.name in self.dynamically_sized_fields
                    and f.variable_length_field.type_model
                ]
            )
        )

    # Get the model types, recursively delving into any fields that are of record type:
    def get_all_types_recursive(self):
        types = []
        for f in self.fields.values():
            types.append(f.type)
            if f.is_packed_type:
                types.extend(f.type_model.get_all_types_recursive())
        return list(OrderedDict.fromkeys(types))

    # Get all type models, recursively delving into any fields that are of record type:
    def get_all_type_models_recursive(self):
        type_models = []
        for f in self.fields.values():
            if f.is_packed_type:
                type_models.append(f.type_model)
                type_models.extend(f.type_model.get_all_type_models_recursive())
        return list(OrderedDict.fromkeys(type_models))

    # Get all enum models recursively delving into any fields that are of record type:
    def get_all_enum_models_recursive(self):
        enum_models = []
        for f in self.fields.values():
            if f.is_packed_type:
                enum_models.extend(f.type_model.get_all_enum_models_recursive())
            elif f.is_enum:
                enum_models.append(f.type_model)
        return list(OrderedDict.fromkeys(enum_models))

    # Returns a flat ordered list of field objects that make up this record:
    def flatten(self):
        from copy import copy

        fields = []
        for a_field in self.fields.values():
            if a_field.is_packed_type:
                fields.extend(a_field.type_model.flatten())
            else:
                fields.append(copy(a_field))
        return fields

    def flattened_names(self, prefix="", separator="."):
        names = []
        for a_field in self.fields.values():
            if a_field.is_packed_type:
                names.extend(
                    a_field.type_model.flattened_names(
                        (prefix if prefix else "") + a_field.name + separator, separator
                    )
                )
            else:
                names.append(prefix + a_field.name)
        return names

    def flatten_dict(self, prefix="", separator="."):
        items = self.flatten()
        names = self.flattened_names(prefix, separator)
        return OrderedDict(zip(names, items))

    def flattened_descriptions(self, prefix="", separator="."):
        descriptions = []
        for a_field in self.fields.values():
            if a_field.is_packed_type:
                descriptions.extend(
                    a_field.type_model.flattened_descriptions(
                        (prefix if prefix else "")
                        + a_field.name
                        + " - "
                        + (a_field.description if a_field.description else "")
                        + separator,
                        separator,
                    )
                )
            else:
                descriptions.append(
                    prefix
                    + a_field.name
                    + " - "
                    + (a_field.description if a_field.description else "")
                )
        return descriptions

    # Override this method so that we can also recursively load and set any type ranges for any
    # packed types that this packed record may contain as fields.
    def load_type_ranges(self):
        # Load the type ranges for any field that this record has that is also a packed
        # type.
        for f in self.fields.values():
            if f.is_packed_type:
                f.type_model.load_type_ranges()

        # Finally, call the base method to load non-packed type fields.
        super(record, self).load_type_ranges()

    def set_type_ranges(self, type_ranges_model):
        # For each field, if type is not a packed record, then set the
        # type range using the value in the type ranges model.
        for f in self.fields.values():
            if (
                not f.type_ranges_loaded
                and not f.is_packed_type
                and not f.format.length
                and not f.skip_validation
            ):
                type_range = type_ranges_model.get_type_by_name(f.type)
                # Assume this is a number with a standard min and max range:
                try:
                    f.range_min = type_range.min
                    f.range_max = type_range.max
                # OK this must be an enumeration instead:
                except AttributeError:
                    f.literals = type_range.literals
                f.type_ranges_loaded = True

    # Returns true if the packed type is always valid, meaning running
    # 'Valid on the type will ALWAYS produce True. In other words, there
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

        # OK, now for each field see if the field can be invalid.
        for f in self.fields.values():
            if not f.is_always_valid():
                return False

        # Everything checks out. The type cannot be invalid:
        return True

    def flattened_names_seq(self, prefix=""):
        names = []
        for a_field in self.fields.values():
            if a_field.is_packed_type:
                names.extend(
                    a_field.type_model.flattened_names(
                        (("." + prefix) if prefix else "") + a_field.name
                    )
                )
            else:
                names.append(a_field.name)
        return names

    # This function will create a graph of all of the child fields of whatever data_product is calling it
    # This function returns a dictionary in the form of:
    # {
    # "paths" : [[]],
    # "graph": {}
    # }
    # Where "paths" is a 2D array that represents all of the paths from the graph (using the graphs keys)
    # and graph is the graph itself (this allows jinja to access all information contained in a node)
    def create_record_graph(self):
        queue = []
        # These lists exist so that we can find all of the paths in the order we care about
        starting_nodes = []
        ending_nodes = []

        record_graph = {}

        # Initiate the queue by finding all the child fields of the calling data product
        queue = queue + search_fields(self)

        firstRun = True
        while len(queue) > 0:
            # Add new nodes from the queue into the record_graph
            for node in queue:
                new_node = fetch_node_template()
                new_node["field"] = node
                if firstRun and node.is_packed_type:
                    starting_nodes.append(node.name)
                elif not node.is_packed_type and node.name not in ending_nodes:
                    ending_nodes.append(node.name)
                record_graph[node.name] = new_node

            # Reset the queue. If nothing is added to the queue after this then we are done searching
            queue = []

            # Search for children of the existing entries in the record_graph
            # If we find any we will add them to the queue and the search will continue
            # This will search some things multiple times, which should be fixed at some point
            for key in record_graph:
                if len(record_graph[key]["children"]) != 0:
                    continue
                try:
                    new_children = search_fields(record_graph[key]["field"].type_model)
                    queue = queue + new_children
                    for child in new_children:
                        record_graph[key]["children"].append(child.name)
                except Exception:
                    continue
            firstRun = False
            # If no new children were found then the queue should be empty and the loop will terminate

        paths = []
        for path in self.flattened_names_seq():
            paths.append(path.split("."))

        return {"paths": paths, "graph": record_graph}


#####################
# Below are helper functions for create_record_graph()
#####################


# Function that returns a list of child fields from a parent
def search_fields(a_record):
    field_list = []
    for a_field in a_record.fields.values():
        field_list.append(a_field)
    return field_list


# A template for an individual node in the data_product graph
def fetch_node_template():
    return {"field": None, "children": []}
