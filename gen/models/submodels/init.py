from models.submodels.parameter import parameter
from models.submodels.subprogram import subprogram
from models.exceptions import throw_exception_with_lineno


# This model holds a component's init() data and allows
# easy access to its members:
class init(subprogram):
    # To initialize the init object, component data must be
    # passed in.
    @throw_exception_with_lineno
    def __init__(self, init_data):
        # Load the discriminant parameters into list:
        parameters = []
        description = None
        if "description" in init_data:
            description = init_data["description"]
        if "parameters" in init_data:
            for par in init_data["parameters"]:
                default_value = None
                if "default" in par and par["default"]:
                    default_value = par["default"]
                par_desc = None
                if "description" in par:
                    par_desc = par["description"]
                parameters.append(
                    parameter(
                        name=par["name"],
                        type=par["type"],
                        value=None,
                        default_value=default_value,
                        description=par_desc,
                    )
                )

        # Instantiate subprogram base class:
        super(init, self).__init__(
            name="init", description=description, parameters=parameters
        )

    @classmethod
    def from_component_data(cls, component_data):
        if "init" in component_data:
            return cls(component_data["init"])
        else:
            return None
