from util import ada
from util import model_loader


class datatype(object):
    def __init__(self, name, description=None):
        self.name = ada.formatType(name)
        self.name_in_package = self.name.split(".")[-1]
        self.description = description
        self.model = None
        self.package = None
        self.generic = False
        self.is_packed_type = False
        self.is_volatile_type = False
        self.is_atomic_type = False
        self.is_register_type = False
        self.is_enum = False
        # set to true when range_min/range_max or literals is set.
        # we don't do this preemptively since it takes a lot of time
        # to set these, and they are rarely needed.
        self.type_ranges_loaded = False
        self.range_min = None
        self.range_max = None
        self.literals = None  # Only populated if an enum type

        # Calculate type package, if one:
        if not ada.isTypePrimitive(self.name):
            self.package = ada.getPackage(self.name)

        # Try to load the model for the datatype:
        if self.package:
            if self.name.endswith(".T") or self.name.endswith(".T_Le"):
                self.model = model_loader.try_load_model_by_name(self.package)
                self.is_packed_type = True
            elif self.name.endswith(".U"):
                self.model = model_loader.try_load_model_by_name(self.package)
            elif self.name.endswith(".E"):
                # Special handling for enumeration type:
                sp = self.package.split(".")
                self.model = model_loader.try_load_model_by_name(sp[0])
                if self.model:
                    self.package = sp[0]
                    self.model = self.model.get_enum_by_name(sp[1])
                    self.is_enum = True
            elif self.name.endswith(".Volatile_T") or self.name.endswith(
                ".Volatile_T_Le"
            ):
                self.model = model_loader.try_load_model_by_name(self.package)
                self.is_packed_type = True
                self.is_volatile_type = True
            elif self.name.endswith(".Atomic_T") or self.name.endswith(".Atomic_T_Le"):
                self.model = model_loader.try_load_model_by_name(self.package)
                self.is_packed_type = True
                self.is_volatile_type = True
                self.is_atomic_type = True
            elif self.name.endswith(".Register_T") or self.name.endswith(
                ".Register_T_Le"
            ):
                self.model = model_loader.try_load_model_by_name(self.package)
                self.is_packed_type = True
                self.is_volatile_type = True
                self.is_register_type = True


class variable(object):
    """Class holding information related to an Ada variable"""
    def __init__(self, name, type, description=None, value=None, default_value=None, not_null=False):
        self.name = ada.formatVariable(name)
        self.datatype = datatype(name=type)
        self.description = description
        # Value of variable, usually set by assembly data:
        self.value = ada.formatValue(value)
        # Default value of variable, usually set by model, and used as default initialization
        # in subprogram definitions:
        self.default_value = ada.formatValue(default_value)
        self.not_null = not_null

    ################################
    # Properties:
    ################################
    @property
    def type(self):
        return self.datatype.name

    @property
    def generic(self):
        return self.datatype.generic

    def set_type(self, type_name):
        generic = self.datatype.generic
        self.datatype = datatype(name=type_name)
        self.datatype.generic = generic

    def set_generic(self, generic=True):
        self.datatype.generic = generic

    @property
    def type_package(self):
        return self.datatype.package

    @property
    def type_model(self):
        return self.datatype.model

    @property
    def is_packed_type(self):
        return self.datatype.is_packed_type

    @property
    def is_volatile_type(self):
        return self.datatype.is_volatile_type

    @property
    def is_register_type(self):
        return self.datatype.is_register_type

    @property
    def is_atomic_type(self):
        return self.datatype.is_atomic_type

    @property
    def is_enum(self):
        return self.datatype.is_enum

    @property
    def range_min(self):
        return self.datatype.range_min

    @property
    def type_ranges_loaded(self):
        return self.datatype.type_ranges_loaded

    @type_ranges_loaded.setter
    def type_ranges_loaded(self, value):
        self.datatype.type_ranges_loaded = value

    @range_min.setter
    def range_min(self, value):
        self.datatype.range_min = value

    @property
    def range_max(self):
        return self.datatype.range_max

    @range_max.setter
    def range_max(self, value):
        self.datatype.range_max = value

    @property
    def literals(self):
        return self.datatype.literals

    @literals.setter
    def literals(self, value):
        self.datatype.literals = value

    ################################
    # String representations:
    ################################
    def declaration_string(self, include_mode=True):
        return (
            self.name
            + " : "
            + ("not null " if self.not_null else "")
            + self.type
            + (" := " + str(self.default_value) if str(self.default_value) else "")
        )

    def call_string(self):
        return (self.name + " => " + str(self.value)) if str(self.value) else ""

    def __repr__(self):
        return self.declaration_string()

    def __str__(self):
        return self.__repr__()
