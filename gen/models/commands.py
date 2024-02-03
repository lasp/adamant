from models.component import component_submodel
from util import ada
from models.submodels.ided_suite import ided_suite, ided_entity
from models.submodels.parameter import parameter
import os.path


class command(ided_entity):
    def __init__(self, name, type=None, description=None, id=None, suite=None):
        super(command, self).__init__(
            name,
            type,
            description,
            id,
            default_value=None,
            variable_types_allowed=True,
            packed_types_required=True,
            suite=suite,
        )

    @classmethod
    def from_ided_entity(cls, entity):
        self = entity
        return self


# This is the object model for a command suite. It extracts data from a
# input file and stores the data as object member variables.
class commands(component_submodel, ided_suite):
    # Initialize the commands object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self, filename=filename, schema=os.environ["SCHEMAPATH"] + "/commands.yaml"
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Commands"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="commands",
            type_name="arg_type",
            variable_types_allowed=True,
            packed_types_required=True,
        )

        # Rename entities to something more descriptive:
        self.commands = list(self.entities.values())

    def set_component(self, component):
        # Set the id bases parameter:
        component.set_id_bases_parameters.append(
            parameter(
                name="command_Id_Base",
                type="Command_Types.Command_Id_Base",
                description="The value at which the component's command identifiers begin.",
            )
        )

        # Store all includes for the component:
        component.base_ads_includes.extend(
            [
                "Basic_Types",
                "Command_Enums",
                "Command_Registration_Request",
                "Command_Response",
            ]
            + self.includes
            + [self.name, "Interfaces", "Command_Types", "Command"]
        )
        if self.basic_types:
            component.base_adb_includes.append("Serializer")
        if self.variable_length_types:
            component.base_adb_includes.append("Serializer_Types")
            component.tester_base_adb_includes.extend(["Serializer_Types"])
        if self.commands is not None:
            component.tester_base_adb_includes.extend(["Command_Types"])
        component.tester_base_adb_includes.extend(
            ["Command_Id.Representation"] + self.representation_includes
        )
        component.base_adb_includes.extend(
            ["Command_Arg_Buffer_Length", "Byte_Array_Util", "Command_Id"]
            + [t + ".Validation" for t in self.complex_type_includes]
        )
        component.tester_base_ads_includes.extend(["Command", self.name])

        # Do base class load:
        component_submodel.set_component(self, component)

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)
