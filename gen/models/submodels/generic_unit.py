from models.exceptions import ModelException, throw_exception_with_lineno
from collections import OrderedDict
from util import ada
import re


class formal_parameter(object):
    """Class holding information related to an Ada variable"""
    def __init__(
        self, name, formal_type=None, description=None, type=None, optional=False
    ):
        self.name = ada.formatType(name)
        self.description = description
        if formal_type:
            self.formal_type = formal_type
        else:
            self.formal_type = "type " + self.name + " is private;"
        self.type = None
        self.type_package = None
        if type:
            self.instantiate(type)
        self.optional = optional
        self.serialized_length_func = None

    @classmethod
    @throw_exception_with_lineno
    def from_parameter_data(cls, parameter_data):
        name = parameter_data["name"]
        description = None
        if "description" in parameter_data:
            description = parameter_data["description"]
        formal_type = None
        if "formal_type" in parameter_data:
            formal_type = parameter_data["formal_type"]
        optional = False
        if "optional" in parameter_data:
            optional = bool(parameter_data["optional"])
        return cls(
            name=name,
            formal_type=formal_type,
            description=description,
            optional=optional,
        )

    ################################
    # String representations:
    ################################
    def call_string(self):
        return self.name + " => " + (self.type if self.type else "TBD")

    def __repr__(self):
        return self.call_string()

    def __str__(self):
        return self.__repr__()

    def instantiate(self, type):
        if self.type:
            raise ModelException(
                "Cannot instantiate formal parameter '"
                + self.name
                + "' with type '"
                + type
                + "'. It is already instantiated as '"
                + self.type
                + "'."
            )

        try:
            # Handle integer formal parameters
            self.type = str(int(type))
        except ValueError:
            # Handle normal formal parameters
            self.type = ada.formatType(type)
            # Calculate type package, if one:
            if not ada.isTypePrimitive(self.type):
                self.type_package = ada.getPackage(self.type)


class generic_unit(object):
    """This class holds data concerning an Ada generic unit"""
    ################################
    # Initialization:
    ################################
    def __init__(self, formal_parameters=[], description=None):
        """
        To initialize the subprogram object, component data must be
        passed in.
        """
        self._description = description
        self._formal_parameters = OrderedDict()

        # Create parameters dict from list:
        for par in formal_parameters:
            if par.name not in self._formal_parameters:
                self._formal_parameters[par.name] = par
            else:
                raise ModelException(
                    "Duplicate formal parameter '" + par.name + "' not allowed."
                )

        # Special handling for Serialized_Length functions that operate on
        # types. We want Adamant to be able to autocode the correct queueing code for
        # variable length types. We only want to do that if there is a formal parameter
        # that exactly matches the Serialized_Length function needed, for a connector type
        # that is also a generic parameter. At this level, we just store an additional list
        # of formal parameter types that also have a serialized length associated with them.
        #
        # The formal parameter we are looking for looks something like this for a type "T":
        #
        # with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural)
        #   return Serializer_Types.Serialization_Status;
        #
        # The regex used below should find any valid Ada (hopefully) that matches this function type.
        # I used the wonderful https://pythex.org/ to figure out a good regex.
        self._variable_types = OrderedDict()
        for par in formal_parameters:
            found = list(
                set(
                    re.findall(
                        (r"with\s+function\s+\S+\s*\(\s*\w+\s*:\s*i*n*\s*(\w+);\s*\w+\s*:\s*out\s*Natural\)"
                         r"\s+return\s+(\w*\.)?Serialization_Status;"),
                        par.formal_type,
                        re.IGNORECASE,
                    )
                )
            )
            variable_type = None
            if found:
                variable_type = ada.formatType(found[0][0])
                if variable_type in self._formal_parameters:
                    self._variable_types[variable_type] = self._formal_parameters[
                        variable_type
                    ]
                    self._formal_parameters[variable_type].serialized_length_func = par

    @staticmethod
    def from_name_value_pairs(name_value_list):
        parameters = []
        if name_value_list:
            for name_value in name_value_list:
                nv = name_value.split("=>")
                if len(nv) != 2:
                    raise ModelException(
                        "'" + str(name_value) + "' is not of format 'name => value'"
                    )
                parameters.append(
                    formal_parameter(name=nv[0].strip(), type=nv[1].strip())
                )
        return generic_unit(formal_parameters=parameters)

    @classmethod
    @throw_exception_with_lineno
    def _from_generic_component_data(cls, generic_component_data):
        generic_parameters = []
        for parameter_data in generic_component_data["parameters"]:
            generic_parameters.append(
                formal_parameter.from_parameter_data(parameter_data)
            )
        if generic_parameters:
            description = None
            if "description" in generic_component_data:
                description = generic_component_data["description"]
            return cls(formal_parameters=generic_parameters, description=description)
        return None

    @classmethod
    def from_component_data(cls, component_data):
        if "generic" in component_data:
            if "parameters" in component_data["generic"]:
                return cls._from_generic_component_data(component_data["generic"])
        return None

    ################################
    # String representations:
    ################################
    def formal_parameter_call_string(self):
        return ", ".join([p.call_string() for p in self._formal_parameters.values()])

    def resolved_formal_parameter_call_string(self):
        return ", ".join(
            [p.call_string() for p in self._formal_parameters.values() if p.type]
        )

    def __repr__(self):
        return self.formal_parameter_call_string()

    def __str__(self):
        return self.__repr__()

    ################################
    # Properties:
    ################################
    @property
    def description(self):
        return self._description

    @property
    def formal_parameters(self):
        return list(self._formal_parameters.values())

    @property
    def variable_length_type_names(self):
        return list(self._variable_types.keys())

    @property
    def formal_parameter_names(self):
        return list(self._formal_parameters.keys())

    @property
    def required_formal_parameter_names(self):
        return [
            k.name for k in list(self._formal_parameters.values()) if not k.optional
        ]

    @property
    def optional_formal_parameter_names(self):
        return [k.name for k in list(self._formal_parameters.values()) if k.optional]

    @property
    def includes(self):
        """Return unique include list."""
        includes = []
        includes.extend(
            [
                par.type_package
                for par in self._formal_parameters.values()
                if par.type_package
            ]
        )
        return list(OrderedDict.fromkeys(includes))

    ################################
    # Methods:
    ################################
    def get_formal_parameter(self, parameter_name):
        parameter_name = ada.formatType(parameter_name)
        if self._formal_parameters and parameter_name in self._formal_parameters:
            return self._formal_parameters[parameter_name]
        else:
            return None

    def get_formal_parameter_type(self, parameter_name):
        par = self.get_formal_parameter(parameter_name)
        if par:
            return par.type
        else:
            raise ModelException(
                "Formal parameter '"
                + parameter_name
                + "' does not exist in generic unit."
            )

    def check_for_missing_instantiations(self):
        """
        Raises exception with appropriate error message if values
        don't check out, or if one is missing.
        """
        missing_vals = []
        for par in self._formal_parameters.values():
            if not par.type and not par.optional:
                missing_vals.append(par.name)
        if missing_vals:
            raise ModelException(
                "Missing instantiation type for formal parameters "
                + str(missing_vals)
                + ". Instantiation of the following formal parameters are required: "
                + str(list(self.required_formal_parameter_names))
            )

    def instantiate_types(self, assembly_component_parameters_data):
        """
        May raise ModelException if the set values do not match the
        names already contained within the object.
        """
        # If we are trying to set values for a component that doesn't have
        # an init, then throw an error.
        if not self._formal_parameters and assembly_component_parameters_data:
            raise ModelException(
                "Generic formal parameters for '" + self._name + "' are not required."
            )

        # We expect the data to be a list of name value pairs in
        # the form "name => value", the following might raise
        # an exception that should be caught.
        nv_gu = generic_unit.from_name_value_pairs(assembly_component_parameters_data)

        # Update the internal dictionary with the new values:
        for param in nv_gu.formal_parameters:
            # Note, this check should be identical to the one in set_parameter_value,
            # just with different error messages to user.
            if param.name in self._formal_parameters:
                if self._formal_parameters[param.name].type:
                    raise ModelException(
                        "Duplicate formal parameter instantiation of '"
                        + param.name
                        + "' not allowed. Already instantiated to '"
                        + self._formal_parameters[param.name].type
                        + "'."
                    )
                else:
                    self._formal_parameters[param.name].instantiate(param.type)
            else:
                raise ModelException(
                    "Formal parameter '"
                    + param.name
                    + "' does not exist in generic unit. The following formal parameters are expected: "
                    + str(list(self.formal_parameter_names))
                    + " and these were provided: "
                    + str(list(nv_gu.formal_parameter_names))
                )
