from util import ada
import os.path
from collections import OrderedDict
from models.exceptions import ModelException, throw_exception_with_filename
from util import model_loader
from models.assembly import assembly_submodel

# Fetch the packet type size from the assembly, and save the result internally
# in case it is asked for again.
packet_obj = [None]


def _get_packet_buffer_size():
    if packet_obj[0] is None:
        packet_obj[0] = model_loader.try_load_model_by_name(
            "Packet", model_types="record"
        )
    if not packet_obj[0]:
        raise ModelException(
            "Could not load model for Packet.T. This must be in the path."
        )
    for fld in packet_obj[0].fields.values():
        if fld.name == "Buffer":
            return fld.size
    assert False, "No field 'Buffer' found in Packet.T type"


# Fetch the parameter table header size from the assembly, and save the result internally
# in case it is asked for again.
param_table_header_obj = [None]


def _get_parameter_table_header_size():
    if param_table_header_obj[0] is None:
        param_table_header_obj[0] = model_loader.try_load_model_by_name(
            "Parameter_Table_Header", model_types="record"
        )
    if not param_table_header_obj[0]:
        raise ModelException(
            "Could not load model for Parameter_Table_Header.T. This must be in the path."
        )
    return param_table_header_obj[0].size


class parameter_table_entry(object):
    def __init__(
        self, component_name, parameter_name, component_model, parameter_model
    ):
        self.component_name = ada.formatVariable(component_name)
        self.parameter_name = ada.formatType(parameter_name)
        self.name = self.component_name + "." + self.parameter_name

        # Variables to be set during resolving of ids.
        self.component = component_model  # the component model
        self.parameter = parameter_model  # the parameter model
        assert parameter_model.type_model, "All parameters must be packed types."
        self.size = parameter_model.type_model.size  # in bytes
        self.start_index = None
        self.end_index = None
        self.component_id = None


# This is the object model for a parameter table. It extracts data from a
# input file and stores the data as object member variables.
class parameter_table(assembly_submodel):
    # Initialize the packet object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(parameter_table, self).__init__(
            filename, schema_dir + "/parameter_table.yaml"
        )

    # Load command specific data structures with information from YAML file.
    def load(self):
        # Load the base class model:
        super(parameter_table, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.parameter_name_list = []
        self.parameter_table_resolved = False
        self.parameters = OrderedDict()  # map from name to packet obj
        self.components = (
            OrderedDict()
        )  # map from component name to component object for all components listed in parameter table

        # Populate the object with the contents of the
        # file data:
        if self.specific_name:
            self.name = ada.formatVariable(self.specific_name)
        else:
            self.name = ada.formatVariable(self.model_name) + "_Parameter_Table"
        if "description" in self.data:
            self.description = self.data["description"]
        self.parameters_instance_name = ada.formatVariable(
            self.data["parameters_instance_name"]
        )

        # Load the parameter list:
        self.parameter_name_list = self.data["parameters"]

    def _resolve_parameter_table(self):
        if self.parameter_table_resolved:
            return
        self.parameter_table_resolved = True
        # The assembly should be loaded first:
        assert (
            self.assembly
        ), "The assembly should be loaded before calling this function."

        def get_component_by_name(component_name):
            comp = self.assembly.get_component_with_name(component_name)
            if not comp:
                raise ModelException(
                    'Parameter table "'
                    + self.name
                    + '" includes component "'
                    + component_name
                    + '" which does not exist in the assembly "'
                    + self.assembly.name
                    + '".'
                )

            # The component is valid, find its parameters:
            if not comp.parameters:
                raise ModelException(
                    'Parameter table "'
                    + self.name
                    + '" includes component "'
                    + component_name
                    + '" which does not have any parameters.'
                )

            # The component is valid and has parameters, return it:
            return comp

        def store_parameter_table_entry(table_entry):
            # Append to parameter table entries:
            if table_entry.name in self.parameters:
                raise ModelException(
                    'Parameter table "'
                    + self.name
                    + '" has parameter "'
                    + table_entry.name
                    + '" that was found in the table more than once. All parameters listed in the table must be unique.'
                )
            self.parameters[table_entry.name] = table_entry
            # Append to component dictionary
            try:
                self.components[
                    table_entry.component.instance_name
                ] = table_entry.component
            except KeyError:
                pass

        # For each parameter listing, load the component and parameter models and form a parameter table entry object
        for parameter_name in self.parameter_name_list:
            split_name = parameter_name.split(".")

            if len(split_name) == 1:
                # This specifies only the component name, so we need to load the component first to grab
                # all of its parameters.
                component_name = split_name[0]
                comp = get_component_by_name(component_name)

                # Load a table entry for each parameter in the component:
                for param in comp.parameters:
                    # Create table entry object
                    table_entry = parameter_table_entry(
                        component_name=component_name,
                        parameter_name=param.name,
                        component_model=comp,
                        parameter_model=param,
                    )

                    # Store table entry in dictionary:
                    store_parameter_table_entry(table_entry)

                # Update dependencies
                self.dependencies.extend(
                    [comp.parameters.full_filename] +
                    comp.parameters.get_dependencies()
                )

            elif len(split_name) == 2:
                # This specifies both component name and parameter name. First load the component:
                component_name = split_name[0]
                parameter_name = split_name[1]
                comp = get_component_by_name(component_name)

                # Now make sure that the parameter specified actually exists in the component:
                param = comp.parameters.get_with_name(parameter_name)
                if not param:
                    raise ModelException(
                        'Parameter table "'
                        + self.name
                        + '" has parameter "'
                        + parameter_name
                        + '" which does not exist in component "'
                        + component_name
                        + '".'
                    )

                # Create table entry object
                table_entry = parameter_table_entry(
                    component_name=component_name,
                    parameter_name=parameter_name,
                    component_model=comp,
                    parameter_model=param,
                )

                # Store table entry in dictionary:
                store_parameter_table_entry(table_entry)

                # Update dependencies
                self.dependencies.extend(
                    [comp.parameters.full_filename] +
                    comp.parameters.get_dependencies()
                )

            else:
                raise ModelException(
                    'Parameter table "'
                    + self.name
                    + '" contains listed parameter that has an invalid name "'
                    + parameter_name
                    + '". Parameter names should be of the format "Component.Parameter_Name" or "Component" '
                    + 'if you want to specify all parameters for that component.'
                )

        # For each parameter table entry object set the start index and end index. This will compact all
        # the parameters together as tight as possible.
        start_index = 0
        for table_entry in self.parameters.values():
            table_entry.start_index = start_index
            assert (
                table_entry.size % 8
            ) == 0, "All packed records should be 8-bit aligned"
            start_index += int(table_entry.size / 8)
            table_entry.end_index = start_index - 1

        # Compute the parameter table size which is the last index computed plus the size of the header:
        self.size = start_index + int(_get_parameter_table_header_size() / 8)
        # Check that size will fit inside a single packet of data, since the component
        # can not dump multiple packets at this time:
        if self.size > (_get_packet_buffer_size() / 8):
            raise ModelException(
                'Parameter table "'
                + self.name
                + '" has size '
                + str(self.size)
                + " bytes, which is larger than the buffer size of a Packet.T, which is "
                + str(int(_get_packet_buffer_size() / 8))
                + " bytes."
            )

        #
        # Now we need to resolve the parameter's component ids:
        #

        # Verify that the parameters component instance that this parameter table is being constructed
        # for actually exists in the assembly and is a Parameters component type.
        self.parameters_instance_model = self.assembly.get_component_with_name(
            self.parameters_instance_name
        )
        if not self.parameters_instance_model:
            raise ModelException(
                'Parameters component instance "'
                + self.parameters_instance_name
                + '" does not exist in the assembly "'
                + self.assembly.name
                + '", so a Parameter Table cannot be built.'
            )
        if not self.parameters_instance_model.name == "Parameters":
            raise ModelException(
                'Parameters component instance "'
                + self.parameters_instance_name
                + '" in assembly "'
                + self.assembly.name
                + '" is not of type "Parameters" so a Parameter Table cannot be built. It is instead of type "'
                + self.parameters_instance_model.name
                + '".'
            )

        # Get the parameters component output connector:
        parameter_connector = self.parameters_instance_model.connectors.of_name(
            "Parameter_Update_T_Provide"
        )
        # Get all the component names connected to that connector:
        connections = parameter_connector.get_connections()
        connected_components = {}
        for idx, c in enumerate(connections):
            if c is not None and c != "ignore":
                try:
                    connected_components[c.to_component.instance_name].append(idx + 1)
                except KeyError:
                    connected_components[c.to_component.instance_name] = [idx + 1]

        # Create map of destination name to indexes that it corresponds to:
        destinations = {}
        for component_name in self.components.keys():
            indexes = [None]
            if component_name in connected_components:
                indexes = connected_components[component_name]
            else:
                raise ModelException(
                    'Parameterized component "'
                    + component_name
                    + '" is not connected to "'
                    + self.parameters_instance_name
                    + '" so a parameter table cannot be produced updates values inside that component.'
                )
            # Add destination map to dict:
            destinations[component_name] = indexes

        # Resolve all parameter component ids:
        for table_entry in self.parameters.values():
            table_entry.component_id = destinations[table_entry.component_name][0]

        # Remove duplicate dependencies
        self.dependencies = list(set(self.dependencies))

    # Public function to resolve all of the parameter ids, given
    # an assembly model.
    @throw_exception_with_filename
    def set_assembly(self, assembly):
        # Make sure an assembly is set by the base class implementation.
        super(parameter_table, self).set_assembly(assembly)

        # Resolve the parameter table now that we have an assembly object.
        self._resolve_parameter_table()
