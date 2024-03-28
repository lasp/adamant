from util import ada
import os.path
from util import model_loader
from models.exceptions import (
    ModelException,
    throw_exception_with_lineno,
    throw_exception_with_filename,
)
from collections import OrderedDict
from models.assembly import assembly_submodel

# Fetch the data product type buffer size and save the result internally
# in case it is asked for again.
packet_obj = [None]


def _get_data_product_buffer_size():
    if packet_obj[0] is None:
        packet_obj[0] = model_loader.try_load_model_by_name(
            "Data_Product", model_types="record"
        )
    if not packet_obj[0]:
        raise ModelException(
            "Could not load model for Data_Product.T. This must be in the path."
        )
    for fld in packet_obj[0].fields.values():
        if fld.name == "Buffer":
            return int(fld.size / 8)
    assert False, "No field 'Buffer' found in Data_Product.T type"


class fault_response(object):
    # To initialize the component object, connector data must be
    # passed in.
    def __init__(
        self,
        fault_name,
        latching,
        startup_state,
        command_response_name,
        command_arg=None,
        description=None,
    ):
        # Store the fault name:
        self.full_fault_name = ada.formatType(fault_name)
        self.full_fault_name_str = self.full_fault_name.replace(".", "_")
        split_name = self.full_fault_name.split(".")
        if len(split_name) != 2:
            raise ModelException(
                "Fault name '"
                + str(self.full_fault_name)
                + "' malformed. Name must be in the form: 'Component_Instance_Name.Fault_Name'."
            )
        self.fault_component_name = split_name[0]
        self.fault_name = split_name[1]
        self.fault_id = None
        self.fault_component = None

        # Store command response name:
        self.full_command_name = ada.formatType(command_response_name)
        split_name = self.full_command_name.split(".")
        if len(split_name) != 2:
            raise ModelException(
                "Command response name '"
                + str(self.full_command_name)
                + "' malformed. Name must be in the form: 'Component_Instance_Name.Command_Name'."
            )
        self.command_component_name = split_name[0]
        self.command_name = split_name[1]
        self.command = None
        self.command_id = None
        self.command_arg_type_model = None
        self.command_arg_length = 0

        # Store everything else:
        self.description = description
        self.startup_state = ada.formatType(startup_state)
        if latching:
            self.latching = "Latching"
        else:
            self.latching = "Non_Latching"
        self.command_arg = command_arg

    @classmethod
    @throw_exception_with_lineno
    def from_fault_response_data(cls, fault_response_data):
        fault_name = fault_response_data["fault"]
        latching = fault_response_data["latching"]
        command_response_name = fault_response_data["command_response"]
        startup_state = fault_response_data["startup_state"]

        description = None
        if "description" in fault_response_data:
            description = fault_response_data["description"]

        command_arg = None
        if "command_arg" in fault_response_data:
            command_arg = fault_response_data["command_arg"]

        return cls(
            fault_name=fault_name,
            latching=latching,
            startup_state=startup_state,
            command_response_name=command_response_name,
            command_arg=command_arg,
            description=description,
        )


# This is the object model for a fault response set. It extracts data from a
# input file and stores the data as object member variables.
class fault_responses(assembly_submodel):
    # Initialize the packet object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(fault_responses, self).__init__(
            filename, schema_dir + "/fault_responses.yaml"
        )

    # Load command specific data structures with information from YAML file.
    def load(self):
        # Load the base class model:
        super(fault_responses, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.responses = []
        self.includes = []
        if "with" in self.data and self.data["with"]:
            self.includes = list(self.data["with"])

        # Populate the object with the contents of the
        # file data:
        if self.specific_name:
            self.name = ada.formatType(self.specific_name)
        else:
            self.name = ada.formatType(self.model_name) + "_Fault_Responses"
        if "description" in self.data:
            self.description = self.data["description"]

        # Load the responses:
        for response_data in self.data["fault_responses"]:
            self.responses.append(
                fault_response.from_fault_response_data(response_data)
            )

        # Make sure size of response table does not exceed the size of the data product needed to hold the statuses of each.
        self.statuses_size = (
            len(self.responses) * 2 + 7
        ) / 8  # Each status is 2 bits in size, round up to nearest byte
        if self.statuses_size > _get_data_product_buffer_size():
            raise ModelException(
                "Statuses data product ("
                + str(self.statuses_size)
                + " bytes) is too large to fit in the buffer of a data product ("
                + str(_get_data_product_buffer_size())
                + " bytes)."
            )

    # Public function to resolve all of the fault and command IDs to their correct values from the names.
    @throw_exception_with_filename
    def resolve_fault_and_command_ids(self, assm):
        self.has_command_args = False
        # Go through all the responses and resolve fault and command IDs:
        for response in self.responses:
            # OK first things first. Let's resolve the fault ID:
            try:
                response.fault_component = assm.components[
                    response.fault_component_name
                ]
            except KeyError:
                raise ModelException(
                    'Component instance "'
                    + response.fault_component_name
                    + '" does not exist in assembly "'
                    + assm.name
                    + '". Cannot build fault response table.'
                )

            # Make sure the component contains a fault suite
            if not response.fault_component.faults:
                raise ModelException(
                    'Component "'
                    + response.fault_component_name
                    + '" does not have any faults. Cannot build fault response table.'
                )

            # Make sure the component contains the specified fault:
            if response.fault_name not in response.fault_component.faults.names():
                raise ModelException(
                    'Component "'
                    + response.fault_component_name
                    + '" does not contain a fault of name "'
                    + response.fault_name
                    + '". It only has faults of the following names: '
                    + str(list(response.fault_component.faults.names()))
                )

            # Set the fault model:
            response.fault = response.fault_component.faults.get_with_name(
                response.fault_name
            )
            response.fault_id = response.fault.id

            # Add fault model to dependencies:
            self.dependencies.extend(
                [response.fault_component.faults.full_filename] +
                response.fault_component.faults.get_dependencies()
            )

            # OK seconds things second. Let's resolve the command ID:
            try:
                response.command_component = assm.components[
                    response.command_component_name
                ]
            except KeyError:
                raise ModelException(
                    'Component instance "'
                    + response.command_component_name
                    + '" does not exist in assembly "'
                    + assm.name
                    + '". Cannot build fault response table.'
                )

            # Make sure the component contains a command suite
            if not response.command_component.commands:
                raise ModelException(
                    'Component "'
                    + response.command_component_name
                    + '" does not have any commands. Cannot build fault response table.'
                )

            # Make sure the component contains the specified command:
            if response.command_name not in response.command_component.commands.names():
                raise ModelException(
                    'Component "'
                    + response.command_component_name
                    + '" does not contain a command of name "'
                    + response.command_name
                    + '". It only has commands of the following names: '
                    + str(list(response.command_component.commands.names()))
                )

            # Set the fault model:
            response.command = response.command_component.commands.get_with_name(
                response.command_name
            )
            response.command_id = response.command.id
            if response.command.type_model:
                response.command_arg_type_model = response.command.type_model
                response.command_arg_length = int(
                    response.command_arg_type_model.size / 8
                )  # in bytes
                self.includes.append(response.command_arg_type_model.name)
                self.has_command_args = True

            # Add command model to dependencies:
            self.dependencies.extend(
                [response.command_component.commands.full_filename] +
                response.command_component.commands.get_dependencies()
            )

        # Uniquify includes:
        self.includes = list(OrderedDict.fromkeys(self.includes))
        self.dependencies = list(set(self.dependencies))
