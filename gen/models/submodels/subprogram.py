from models.exceptions import ModelException
from models.submodels.parameter import parameter
from collections import OrderedDict
from util import ada


class subprogram(object):
    """This class holds data concerning an Ada subprogram"""
    ################################
    # Initialization:
    ################################
    def __init__(self, name, description=None, parameters=[], return_value=None):
        """
        To initialize the subprogram object, component data must be
        passed in.
        """
        self._name = ada.formatType(name)
        self._description = description
        self._return_value = return_value

        # Create parameters dict from list:
        self._parameters = OrderedDict()
        for par in parameters:
            if par.name not in self._parameters:
                self._parameters[par.name] = par
            else:
                raise ModelException(
                    "Duplicate parameter '" + par.name + "' not allowed."
                )

        # Generate includes list:
        includes = []
        representation_includes = []
        if self._return_value and self._return_value.type_package:
            includes.append(self._return_value.type_package)
            if self._return_value.type_model:
                representation_includes.append(
                    self._return_value.type_package + ".Representation"
                )
            else:
                representation_includes.append(self._return_value.type_package)
        includes.extend(
            [par.type_package for par in self._parameters.values() if par.type_package]
        )
        representation_includes.extend(
            [
                par.type_package + ".Representation"
                if par.type_model
                else par.type_package
                for par in self._parameters.values()
                if par.type_package
            ]
        )
        self._includes = list(OrderedDict.fromkeys(includes))
        self._representation_includes = list(
            OrderedDict.fromkeys(representation_includes)
        )

    @staticmethod
    def from_name_value_pairs(name_value_list):
        parameters = []
        if name_value_list:
            for name_value in name_value_list:
                nv = name_value.split("=>")
                if len(nv) < 2:
                    raise ModelException(
                        "'" + str(name_value) + "' is not of format 'name => value'"
                    )
                parameters.append(
                    parameter(
                        name=nv[0].strip(), type="na", value="=>".join(nv[1:]).strip()
                    )
                )
        return subprogram(name="subprogram_values", parameters=parameters)

    ################################
    # String representations:
    ################################
    def parameter_declaration_string(self, include_mode=True):
        return "; ".join(
            [
                p.declaration_string(include_mode=include_mode)
                for p in self._parameters.values()
            ]
        )

    def parameter_call_string(self):
        return ", ".join(
            [p.call_string() for p in self._parameters.values() if p.value]
        )

    def __repr__(self):
        return self.parameter_declaration_string()

    def __str__(self):
        return self.__repr__()

    ################################
    # Properties:
    ################################
    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, name):
        self._name = name

    @property
    def description(self):
        return self._description

    @property
    def return_value(self):
        return self._return_value

    @property
    def return_type(self):
        if self._return_value:
            return self._return_value.type
        return None

    @property
    def return_type_package(self):
        if self._return_value:
            return self._return_value.type_package
        return None

    @property
    def return_type_model(self):
        if self._return_value:
            return self._return_value.type_model
        return None

    @property
    def parameters(self):
        return list(self._parameters.values())

    @property
    def parameter_names(self):
        return list(self._parameters.keys())

    @property
    def required_parameter_names(self):
        """Return all parameter names without a default value set."""
        return [key for key, val in self._parameters.items() if not val.default_value]

    @property
    def not_required_parameter_names(self):
        """Return all parameter names with a default value set."""
        return [key for key, val in self._parameters.items() if val.default_value]

    @property
    def includes(self):
        """Return unique include list."""
        return self._includes

    @property
    def types(self):
        """Return all the types used in the subprogram"""
        types = []
        for par in self.parameters:
            if par.type:
                types.append(par.type)
        if self._return_value:
            types.append(self._return_value.type)
        return list(OrderedDict.fromkeys(types))

    @property
    def basic_types(self):
        types = []
        for par in self.parameters:
            if par.type and not par.type_model:
                types.append(par.type)
        if self._return_value and not self._return_value.type_model:
            types.append(self._return_value.type)
        return types

    @property
    def type_models(self):
        """Return all the type models used in the subprogram"""
        type_models = []
        for par in self.parameters:
            if par.type_model:
                type_models.append(par.type_model)
        if self._return_value and self._return_value.type_model:
            type_models.append(self._return_value.type_model)
        return list(OrderedDict.fromkeys(type_models))

    ################################
    # Methods:
    ################################
    def get_parameter(self, parameter_name):
        parameter_name = ada.formatVariable(parameter_name)
        if self._parameters and parameter_name in self._parameters:
            return self._parameters[parameter_name]
        else:
            return None

    def set_parameter_value(self, parameter_name, value, override=False):
        parameter_name = ada.formatVariable(parameter_name)
        if parameter_name in self._parameters:
            if not override and self._parameters[parameter_name].value:
                raise ModelException(
                    "Parameter '"
                    + parameter_name
                    + "' already set to '"
                    + str(self._parameters[parameter_name].value)
                    + "'. Cannot set to '"
                    + str(value)
                    + "'."
                )
            else:
                self._parameters[parameter_name].value = value
        else:
            raise ModelException(
                "Parameter '"
                + parameter_name
                + "' does not exist in '"
                + self._name
                + "'."
            )

    def get_parameter_value(self, parameter_name):
        par = self.get_parameter(parameter_name)
        if par:
            return par.value
        else:
            raise ModelException(
                "Parameter '"
                + parameter_name
                + "' does not exist in '"
                + self._name
                + "'."
            )

    def check_for_missing_parameter_values(self):
        """
        Raises exception with appropriate error message if values
        don't check out, or if one is missing.
        """
        missing_vals = []
        for par in self._parameters.values():
            if not par.value and not par.default_value:
                missing_vals.append(par.name)
        if missing_vals:
            raise ModelException(
                "Missing parameters "
                + str(missing_vals)
                + " for '"
                + self._name
                + "'. The following parameters are required: "
                + str(list(self.required_parameter_names))
            )

    def set_values(self, assembly_component_parameters_data):
        """
        May raise ModelException if the set values do not match the
        names already contained within the object.
        """
        # If we are trying to set values for a component that doesn't have
        # an init, then throw an error.
        if not self._parameters and assembly_component_parameters_data:
            raise ModelException(
                "Parameters for '" + self._name + "' are not required."
            )

        # We expect the data to be a list of name value pairs in
        # the form "name => value", the following might raise
        # an exception that should be caught.
        nv_subp = subprogram.from_name_value_pairs(assembly_component_parameters_data)

        # Update the internal dictionary with the new values:
        for param in nv_subp.parameters:
            # Note, this check should be identical to the one in set_parameter_value,
            # just with different error messages to user.
            if param.name in self._parameters:
                if self._parameters[param.name].value:
                    raise ModelException(
                        "Duplicate parameter '" + param.name + "' not allowed."
                    )
                else:
                    self._parameters[param.name].value = param.value
            else:
                raise ModelException(
                    "Parameter '"
                    + param.name
                    + "' does not exist in '"
                    + self._name
                    + "'. The following parameters are expected: "
                    + str(list(self.required_parameter_names))
                    + " and these were provided: "
                    + str(list(nv_subp.parameter_names))
                )
