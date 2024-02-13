from models.component import component_submodel
from util import ada
from models.submodels.ided_suite import ided_suite, ided_entity
from models.submodels.parameter import parameter
import os.path
from collections import OrderedDict


class fault(ided_entity):
    def __init__(self, name, type, description=None, id=None, suite=None):
        super(fault, self).__init__(
            name,
            type,
            description,
            id,
            default_value=None,
            variable_types_allowed=False,
            suite=suite,
        )

    @classmethod
    def from_ided_entity(cls, entity):
        self = entity
        return self


# This is the object model for a fault suite. It extracts data from a
# input file and stores the data as object member variables.
class faults(component_submodel, ided_suite):
    # Initialize the faults object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self, filename=filename, schema=os.environ["SCHEMAPATH"] + "/faults.yaml"
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Faults"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="faults",
            type_name="param_type",
        )

        # If the ids for the packets have all been defined then sort the packets by id:
        if len(self.ids) > 0:
            self.entities = OrderedDict(
                sorted(self.entities.items(), key=lambda x: x[1].id)
            )

        # Rename entities to something more descriptive:
        self.faults = list(self.entities.values())

    def _set_component_no_id_bases(self, component):
        # Add includes to component:
        component.base_ads_includes.extend([self.name])
        if not self.ids:
            component.base_ads_includes.extend(["Fault_Types"])
        component.tester_base_ads_includes.extend([self.name, "Fault"] + self.includes)
        if self.basic_types:
            component.tester_base_adb_includes.extend(["Serializer"])
        component.tester_base_adb_includes.extend(self.representation_includes)
        component.tester_template_ads_includes.extend(
            ["Printable_History", "Fault"] + self.representation_includes
        )

        # Do base class load:
        component_submodel.set_component(self, component)

    def set_component(self, component):
        # Set the id bases parameter:
        if not self.ids:
            component.set_id_bases_parameters.append(
                parameter(
                    name="Fault_Id_Base",
                    type="Fault_Types.Fault_Id_Base",
                    description="The value at which the component's fault identifiers begin.",
                )
            )

        self._set_component_no_id_bases(component)

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)

    # Override this method. If we have a fault suite with static ids, then we do not want to call the base class,
    # otherwise we do.
    def set_id_base(self, start_id):
        if not self.ids:
            ided_suite.set_id_base(self, start_id)
