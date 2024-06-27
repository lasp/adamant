from models.exceptions import ModelException, throw_exception_with_lineno
from models.submodels.parameter import parameter
from util import ada
from collections import OrderedDict


class interrupt(object):
    """This class holds data concerning a parameter."""
    def __init__(self, name, description=None, priority=None, id=None):
        self.name = ada.formatVariable(name)
        self.description = description
        self.priority = priority
        self.priority_rank = None
        self.component_name = None
        self.id = id

    @property
    def id_parameter_name(self):
        return self.name + "_Id"

    @property
    def priority_parameter_name(self):
        return self.name + "_Priority"

    def get_id_parameter(self, value=None):
        return parameter(
            name=self.id_parameter_name,
            type="Ada.Interrupts.Interrupt_Id",
            value=value,
            description="Interrupt identifier number for " + self.name,
        )

    def get_priority_parameter(self, value=None):
        return parameter(
            name=self.priority_parameter_name,
            type="System.Interrupt_Priority",
            value=value,
            description="Interrupt priority for " + self.name,
        )


class interrupts(object):
    @throw_exception_with_lineno
    def __init__(self, interrupt_data):
        """
        To initialize the interrupts object, component data must be
        passed in.
        """
        # Load interrupt list into internal dictionary
        self._interrupts = OrderedDict()

        for inter in interrupt_data:
            int_obj = interrupt(
                name=inter["name"],
                description=(inter["description"] if "description" in inter else None),
            )
            if int_obj.name not in self._interrupts:
                self._interrupts[int_obj.name] = int_obj
            else:
                raise ModelException(
                    "Duplicate interrupt '" + int_obj.name + "' not allowed."
                )

    @classmethod
    def from_component_data(cls, component_data):
        if "interrupts" in component_data:
            return cls(component_data["interrupts"])
        else:
            return None

    def __nonzero__(self):
        return bool(self._interrupts)

    def __iter__(self):
        return iter(self._interrupts.values())

    def __len__(self):
        return len(self._interrupts)

    ################################
    # Properties:
    ################################
    @property
    def list(self):
        return self._interrupts.values()

    @property
    def names(self):
        return self._interrupts.keys()

    ################################
    # Methods:
    ################################
    # Set all interrupt ids and priorities given a discriminant object from an assembly model
    def set_ids_and_priorities(self, discriminant_obj, component_name):
        # Resolve all interrupt ids and priorities with the discriminant object:
        for interrupt in self.list:
            # Get the priority and id parameters from the discriminant:
            priority_param = discriminant_obj.get_parameter(
                interrupt.priority_parameter_name
            )
            id_param = discriminant_obj.get_parameter(interrupt.id_parameter_name)
            assert (
                priority_param and id_param
            ), "These parameters must exist or there is a bug."

            # Set the interrupts priority and id:
            interrupt.priority = priority_param.value
            interrupt.id = id_param.value
            interrupt.component_name = component_name
