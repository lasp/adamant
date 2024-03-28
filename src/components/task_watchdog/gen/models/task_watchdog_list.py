from util import ada
import os.path
from models.exceptions import ModelException, throw_exception_with_filename
from collections import OrderedDict
from models.assembly import assembly_submodel


class task_watchdog_entry(object):
    def __init__(self, petter):
        if len(petter["connector_name"].split(".")) != 2:
            raise ModelException(
                "Connector name provided needs to be in the format Component_Instance_Name.Connector_Name: ",
                petter["connector_name"],
            )

        self.component_name = ada.formatVariable(petter["connector_name"].split(".")[0])
        self.connector_name = petter["connector_name"]

        if "name" in petter:
            self.name = ada.formatVariable(petter["name"])
            self.product_name = self.name
        else:
            self.name = ""
            self.product_name = ada.formatVariable(petter["connector_name"]).replace(
                ".", "_"
            )

        self.connector_id = 0

        self.tick_limit = petter["limit"]
        if petter["action"] not in ["error_fault", "warn", "disabled"]:
            raise ModelException("Action not part of the prescribed enum")
        # Check that if the action is as a fault, that it has a fault id as well.
        if petter["action"] in "error_fault" and "fault_id" not in petter:
            raise ModelException(
                "Action was declared as fault, but no fault Id was included: "
                + self.connector_name
            )

        if "fault_id" in petter:
            self.fault_id = petter["fault_id"]
            self.has_fault = True
        else:
            self.fault_id = 0
            self.has_fault = False

        self.action = ada.formatVariable(petter["action"])
        self.critical = petter["critical"]

        if "description" in petter:
            self.description = petter["description"]
        else:
            self.description = ""


# This is the object model for the ccsds_downsampler. It extracts data from a
# input file and stores the data as object member variables.
class task_watchdog_list(assembly_submodel):
    # Initialize the products object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(task_watchdog_list, self).__init__(
            filename, schema_dir + "/task_watchdog_list.yaml"
        )

    def load(self):
        # Load the base class model:
        super(task_watchdog_list, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.task_watchdog_instance_name = None
        self.pets_resolved = False
        self.watchdog_list = OrderedDict()  # map from name to packet obj
        self.num_petters = 0
        self.component_list = (
            OrderedDict()
        )  # map from component name to component object for all components listed in the watchdog pet list
        self.final_watchdog_list = OrderedDict()

        if self.specific_name:
            self.name = ada.formatVariable(self.specific_name)
        else:
            self.name = ada.formatType(self.model_name) + "_Task_Watchdog_List"

        if "description" in self.data:
            self.description = self.data["description"]

        # Find every petter in the list provided by the user
        for petter in self.data["petters"]:
            self.num_petters = self.num_petters + 1
            pet_info = task_watchdog_entry(petter)
            # Make sure there are not duplicate names
            if pet_info.connector_name not in self.watchdog_list:
                self.watchdog_list[pet_info.connector_name] = pet_info
            else:
                raise ModelException(
                    "Cannot have duplicate connectors from the same component: "
                    + str(pet_info.connector_name)
                )

    def _resolve_pet_connectors(self):
        if self.pets_resolved:
            return
        self.pets_resolved = True
        # The assembly should be loaded first:
        assert (
            self.assembly
        ), "The assembly should be loaded before calling this function."

        for assm_component in self.assembly.components.values():
            if "Task_Watchdog" in assm_component.name:
                self.component_list[assm_component.name] = assm_component

        for watchdog_component in self.component_list:
            self.task_watchdog_instance_model = self.assembly.get_component_with_name(
                watchdog_component + "_Instance"
            )
            if not self.task_watchdog_instance_model:
                raise ModelException(
                    'Task watchdog component instance "'
                    + watchdog_component
                    + '" does not exist in the assembly "'
                    + self.assembly.name
                    + '", so a watchdog list cannot be built.'
                )
            if not self.task_watchdog_instance_model.name == "Task_Watchdog":
                raise ModelException(
                    'Task watchdog component instance "'
                    + watchdog_component
                    + '" in assembly "'
                    + self.assembly.name
                    + '" is not of type "Task_Watchdog". It is instead of type "'
                    + self.parameters_instance_model.name
                    + '".'
                )

        # Get the parameters component output connector:
        task_watchdog_connector = self.task_watchdog_instance_model.connectors.of_name(
            "Pet_T_Recv_Sync"
        )
        # Add model to dependencies:
        self.dependencies.append(
            self.task_watchdog_instance_model.full_filename
        )
        # Get all the component names connected to that connector:
        connections = task_watchdog_connector.get_connections()
        connected_components = {}
        assembly_connected_components = 0
        for c in connections:
            assembly_connected_components = assembly_connected_components + 1
            if c is not None and c != "ignore":
                if len(c) > 1:
                    raise ModelException(
                        "The Task_Watchdog only allows a single invoker to be connected to each invokee. Index "
                        + str(c[0].to_index)
                        + " had more than one connection: "
                        + str([conn.name for conn in c])
                    )
                c = c[0]
                key_name = (
                    str(c.from_component.instance_name)
                    + "."
                    + str(c.from_connector.name)
                )
                if key_name not in connected_components:
                    connected_components[key_name] = [c.to_index]
                    # Add model to dependencies:
                    self.dependencies.append(
                        c.from_component.full_filename
                    )
                else:
                    raise ModelException("Connector name already exist: " + key_name)

        if self.num_petters != assembly_connected_components:
            raise ModelException(
                "Task_Watchdog Lists and the assembly do not match on the number of components connected.\n"
                + "Number petters in model: "
                + str(self.num_petters)
                + "\nNumber of connected components found in assembly: "
                + str(assembly_connected_components)
            )
        # Resolve all pet connector component ids:
        for pet_info in self.watchdog_list.values():
            pet_info.connector_id = connected_components[pet_info.connector_name]
            self.watchdog_list[pet_info.connector_name] = pet_info

        # Sort the watchdog list by index:
        self.watchdog_list = OrderedDict(
            sorted(self.watchdog_list.items(), key=lambda x: x[1].connector_id)
        )

        # Remove duplicate dependencies
        self.dependencies = list(set(self.dependencies))

    # Public function to resolve all of the connector ids, given
    # an assembly model.
    @throw_exception_with_filename
    def set_assembly(self, assembly):
        # Make sure an assembly is set by the base class implementation.
        super(task_watchdog_list, self).set_assembly(assembly)

        # Resolve the connectors now that we have an assembly object.
        self._resolve_pet_connectors()
