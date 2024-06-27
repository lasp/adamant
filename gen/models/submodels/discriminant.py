from models.submodels.parameter import parameter
from models.submodels.subprogram import subprogram
from models.submodels.interrupts import interrupts
from models.exceptions import ModelException


class discriminant(subprogram):
    """
    This model holds a component's discriminant data and allows
    easy access to its members:
    """
    def __init__(self, component_data, the_interrupts):
        """
        To initialize the discriminant object, component data must be
        passed in.
        """
        try:
            # Fill in data:
            self._data = component_data

            # Load the discriminant parameters into list:
            parameters = []
            description = None
            if "discriminant" in self._data:
                if "description" in self._data["discriminant"]:
                    description = self._data["discriminant"]["description"]
                if "parameters" in self._data["discriminant"]:
                    for par in self._data["discriminant"]["parameters"]:
                        par_not_null = False
                        if "not_null" in par:
                            par_not_null = par["not_null"]
                        parameters.append(
                            parameter(
                                name=par["name"],
                                type=par["type"],
                                value=None,
                                description=par["description"],
                                not_null=par_not_null,
                            )
                        )

            # Extract any interrupts from the
            # component data and append the resulting
            # discriminant parameters to the list:
            if the_interrupts:
                for interrupt in the_interrupts.list:
                    parameters.append(interrupt.get_id_parameter())
                    parameters.append(interrupt.get_priority_parameter())

            # Instantiate subprogram base class:
            super(discriminant, self).__init__(
                name="discriminant", description=description, parameters=parameters
            )

        except ModelException as e:
            # Set reasonable line number
            if "discriminant" in self._data:
                e.lineno = self._data["discriminant"].lc.line
            else:
                e.lineno = self._data.lc.line
            raise e

    @classmethod
    def from_component_data(cls, component_data):
        ints = interrupts.from_component_data(component_data)
        if ints or "discriminant" in component_data:
            return cls(component_data, ints)
        else:
            return None
