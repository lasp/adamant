from models.component import component_submodel
from util import ada
from models.submodels.ided_suite import ided_suite
from models.submodels.parameter import parameter
import os.path


# This is the object model for an event suite. It extracts data from a
# input file and stores the data as object member variables.
class events(component_submodel, ided_suite):
    # Initialize the events object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self, filename=filename, schema=os.environ["SCHEMAPATH"] + "/events.yaml"
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Events"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="events",
            type_name="param_type",
            packed_types_required=True,
        )

        # Rename entities to something more descriptive:
        self.events = list(self.entities.values())

    def set_component(self, component):
        # Set the id bases parameter:
        component.set_id_bases_parameters.append(
            parameter(
                name="event_Id_Base",
                type="Event_Types.Event_Id_Base",
                description="The value at which the component's event identifiers begin.",
            )
        )

        # Add includes to component:
        component.base_ads_includes.extend([self.name, "Event_Types"])
        component.tester_base_ads_includes.extend([self.name, "Event"] + self.includes)
        if self.basic_types:
            component.tester_base_adb_includes.extend(["Serializer"])
        component.tester_base_adb_includes.extend(self.representation_includes)
        component.tester_template_ads_includes.extend(
            ["Printable_History", "Event"] + self.representation_includes
        )

        # Do base class load:
        component_submodel.set_component(self, component)

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)
