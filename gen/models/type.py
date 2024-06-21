from models.packed_type import packed_type
from models.submodels.field import field
from models.exceptions import ModelException
from util import ada
from collections import OrderedDict
import os


class type(packed_type):
    """
    This is the object model for a packed type. It extracts data from a
    input file and stores the data as object member variables.
    """
    def __init__(self, filename, template=os.environ["SCHEMAPATH"] + "/type.yaml"):
        """
        Initialize the packed type object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        super(type, self).__init__(filename, template=template)

    def _load(self):
        """Load record specific data structures with information from YAML file."""
        # Initialize object members:
        self.element = None

        # Populate the object with the contents of the
        # file data:
        # The type element is almost a field type. We just need to add a name:
        self.data["name"] = self.name
        try:
            self.element = field.from_field_data(
                start_bit=0, field_number=1, field_data=self.data
            )
        except ModelException as e:
            raise ModelException(
                "Error encountered loading data for element: " + str(e)
            )
        self.num_fields = 1

        # Calculate total size:
        self.size = self.element.size

        # Resolve a variable length element:
        self.variable_length = False
        if self.element.variable_length:
            self.variable_length = True
        elif self.element.is_packed_type and self.element.type_model.variable_length:
            self.variable_length = True

        # Store useful lists:

        # Includes:
        self.includes = list(set(self.includes))
        # Store all types that have a model associated with them:
        self.complex_types = [self.element.type] if self.element.is_packed_type else []

        # Store all types that have a model associated with them:
        self.complex_type_models = (
            [self.element.type_model] if self.element.is_packed_type else []
        )
        self.all_type_models = (
            [self.element.type_model] if self.element.type_model else []
        )

        # Store all types that do NOT have a model associated with them:
        self.simple_types = [self.element.type] if not self.element.type_model else []
        self.simple_type_includes = (
            [self.element.type_package]
            if not self.element.type_model and self.element.type_package
            else []
        )
        self.unpacked_types = (
            [self.element.type] if not self.element.is_packed_type else []
        )

        # Store the filenames of all the type models used in this record.
        self.deps_list = (
            [self.element.type_model.full_filename] if self.element.type_model else []
        )

        # Store the includes necessary to include the field types:
        self.type_includes = (
            [self.element.type_package]
            if not ada.isTypePrimitive(self.element.type)
            else []
        )

        # Store the includes for any complex types (those that have models).
        self.modeled_type_includes = (
            [self.element.type_package] if self.element.type_model else []
        )
        self.packed_type_includes = (
            [self.element.type_package] if self.element.is_packed_type else []
        )
        self.unpacked_type_includes = list(
            [self.element.type_package] if not self.element.is_packed_type else []
        )

    def get_all_types_recursive(self):
        """Get the model types, recursively delving into any types that are of record type."""
        types = []
        types.append(self.element.type)
        if self.element.type_model:
            types.extend(self.element.type_model.get_all_types_recursive())
        return list(OrderedDict.fromkeys(types))

    def get_all_type_models_recursive(self):
        """Get all type models, recursively delving into any types that are of record type."""
        type_models = []
        if self.element.is_packed_type:
            type_models.append(self.element.type_model)
            type_models.extend(self.element.type_model.get_all_type_models_recursive())
        return list(OrderedDict.fromkeys(type_models))

    def get_all_enum_models_recursive(self):
        enum_models = []
        if self.element.is_packed_type:
            enum_models.extend(self.element.type_model.get_all_enum_models_recursive())
        elif self.element.is_enum:
            enum_models.append(self.element.type_model)
        return list(OrderedDict.fromkeys(enum_models))

    def set_type_ranges(self, type_ranges_model):
        if not self.element.is_packed_type and not self.element.format.length:
            # Get the type range for the array's type.
            type_range = type_ranges_model.get_type_by_name(self.element.type)

            # Set the element's range:
            # Assume this is a number with a standard min and max range:
            try:
                self.element.range_min = type_range.min
                self.element.range_max = type_range.max
            # OK this must be an enumeration instead:
            except AttributeError:
                self.element.literals = type_range.literals
