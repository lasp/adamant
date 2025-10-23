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


class parameter(object):
    """Represents a single parameter within a parameter table entry."""
    def __init__(
        self, component_name, parameter_name, component_model, parameter_model
    ):
        self.component_name = ada.formatVariable(component_name)
        self.parameter_name = ada.formatType(parameter_name)
        self.name = self.component_name + "." + self.parameter_name

        # The component and parameter models
        self.component = component_model  # the component model
        self.parameter = parameter_model  # the parameter model
        assert parameter_model.type_model, "All parameters must be packed types."

        # Component ID to be set during resolving
        self.component_id = None  # Component ID this parameter belongs to


class parameter_table_entry(object):
    """
    Represents an entry in the parameter table. Each entry has a unique Entry_ID
    (its index in the table), start/end indices in the table, and a list of parameters
    that share this entry. For grouped parameters, multiple parameters share the same
    entry (union), but each parameter has a unique ID and Component_ID.
    """
    def __init__(self, entry_id, parameters):
        """
        Initialize a parameter table entry.

        Args:
            entry_id: Unique entry ID (index into the parameter table)
            parameters: Either a single parameter object or a list of parameter objects
        """
        # Unique entry ID (index into the parameter table)
        self.entry_id = entry_id

        # Convert single parameter to list for uniform handling
        if not isinstance(parameters, list):
            parameters = [parameters]

        # Validate that all parameters have packed types
        for param in parameters:
            assert param.parameter.type_model, "All parameters must be packed types."

        # Validate that all parameters in the group have the same type and size
        if len(parameters) > 1:
            first_param = parameters[0]
            first_type = first_param.parameter.type_model.name
            first_size = first_param.parameter.type_model.size

            for param in parameters[1:]:
                param_type = param.parameter.type_model.name
                param_size = param.parameter.type_model.size

                if param_type != first_type or param_size != first_size:
                    raise ModelException(
                        f"Cannot group parameters of different types/sizes. "
                        f"Parameter '{first_param.name}' has type '{first_type}' (size {first_size}), "
                        f"but parameter '{param.name}' has type '{param_type}' (size {param_size})."
                    )

        # Store the list of parameter objects
        self.parameters = parameters

        # Get size from the first parameter (all parameters in entry have same size)
        self.size = parameters[0].parameter.type_model.size  # in bits

        # Indices to be set during table resolution
        self.start_index = None
        self.end_index = None

        # Name for this entry (use first parameter's name)
        self.name = parameters[0].name


class parameter_table(assembly_submodel):
    """
    This is the object model for a parameter table. It extracts data from a
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the packet object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(parameter_table, self).__init__(
            filename, schema_dir + "/parameter_table.yaml"
        )

    def load(self):
        """Load command specific data structures with information from YAML file."""
        # Load the base class model:
        super(parameter_table, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.parameter_name_list = []
        self.parameter_table_resolved = False
        self.parameters = OrderedDict()  # map from name to parameter_table_entry obj
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

        def create_parameter_from_name(param_name, allow_component_only=True):
            """Helper function to create a parameter object from a parameter name."""
            split_name = param_name.split(".")

            if len(split_name) == 1:
                if not allow_component_only:
                    raise ModelException(
                        'Parameter table "'
                        + self.name
                        + '" has a grouped parameter "'
                        + param_name
                        + '" that is not in the format "Component.Parameter_Name". '
                        + 'Grouped parameters cannot use the shorthand "Component" format to include all parameters.'
                    )
                # Just component name - return all parameters in the component
                component_name = split_name[0]
                comp = get_component_by_name(component_name)
                # Update dependencies
                dependencies.extend(
                    [comp.parameters.full_filename] +
                    comp.parameters.get_dependencies()
                )
                # Return list of parameter objects for all parameters in component
                return [
                    parameter(
                        component_name=component_name,
                        parameter_name=param_model.name,
                        component_model=comp,
                        parameter_model=param_model,
                    )
                    for param_model in comp.parameters
                ]

            elif len(split_name) == 2:
                # Component.Parameter format
                component_name = split_name[0]
                parameter_name = split_name[1]
                comp = get_component_by_name(component_name)

                # Make sure the parameter exists in the component
                param_model = comp.parameters.get_with_name(parameter_name)
                if not param_model:
                    raise ModelException(
                        'Parameter table "'
                        + self.name
                        + '" has parameter "'
                        + parameter_name
                        + '" which does not exist in component "'
                        + component_name
                        + '".'
                    )

                # Update dependencies
                dependencies.extend(
                    [comp.parameters.full_filename] +
                    comp.parameters.get_dependencies()
                )

                # Return single parameter as a list
                return [parameter(
                    component_name=component_name,
                    parameter_name=parameter_name,
                    component_model=comp,
                    parameter_model=param_model,
                )]

            else:
                raise ModelException(
                    'Parameter table "'
                    + self.name
                    + '" contains listed parameter that has an invalid name "'
                    + param_name
                    + '". Parameter names should be of the format "Component.Parameter_Name" or "Component" '
                    + 'if you want to specify all parameters for that component.'
                )

        def store_parameter_table_entry(table_entry):
            """Store a parameter table entry and update component dictionary."""
            if table_entry.name in self.parameters:
                raise ModelException(
                    'Parameter table "'
                    + self.name
                    + '" has parameter "'
                    + table_entry.name
                    + '" that was found in the table more than once. All parameters listed in the table must be unique.'
                )
            self.parameters[table_entry.name] = table_entry
            # Append to component dictionary for all parameters in this entry
            for param in table_entry.parameters:
                try:
                    self.components[param.component.instance_name] = param.component
                except KeyError:
                    pass

        # Track dependencies as we go
        dependencies = []

        # Track all parameters seen across the entire table to detect duplicates
        seen_parameters = {}  # maps parameter name to entry_id where it was first seen

        # For each parameter listing, load the component and parameter models and form a parameter table entry object
        entry_id = 0  # Unique entry ID counter
        for parameter_entry in self.parameter_name_list:
            # Determine if this is a grouped entry (list) or single entry (string)
            if isinstance(parameter_entry, list):
                # Grouped parameters - process all names in the list
                # Grouped parameters must use "Component.Parameter" format (not just "Component")
                param_objs = []
                seen_in_group = {}  # Track parameters within this group to detect duplicates

                for param_name in parameter_entry:
                    new_params = create_parameter_from_name(param_name, allow_component_only=False)

                    # Check for duplicates within this group
                    for param in new_params:
                        if param.name in seen_in_group:
                            raise ModelException(
                                'Parameter table "'
                                + self.name
                                + '" has parameter "'
                                + param.name
                                + '" listed more than once in the same grouped entry (Entry_ID '
                                + str(entry_id)
                                + '). Each parameter can only appear once per group.'
                            )
                        seen_in_group[param.name] = True

                        # Check for duplicates across the entire table
                        if param.name in seen_parameters:
                            raise ModelException(
                                'Parameter table "'
                                + self.name
                                + '" has parameter "'
                                + param.name
                                + '" listed more than once. It first appears in Entry_ID '
                                + str(seen_parameters[param.name])
                                + ' and again in Entry_ID '
                                + str(entry_id)
                                + '. All parameters listed in the table must be unique.'
                            )
                        seen_parameters[param.name] = entry_id

                    param_objs.extend(new_params)

                # Create table entry with all parameters (validation happens in __init__)
                table_entry = parameter_table_entry(entry_id, param_objs)
                store_parameter_table_entry(table_entry)
                entry_id += 1

            else:
                # Single parameter entry (may expand to multiple entries if just component name is given)
                param_objs = create_parameter_from_name(parameter_entry, allow_component_only=True)

                # Create one table entry for each parameter object
                for param_obj in param_objs:
                    # Check for duplicates across the entire table
                    if param_obj.name in seen_parameters:
                        raise ModelException(
                            'Parameter table "'
                            + self.name
                            + '" has parameter "'
                            + param_obj.name
                            + '" listed more than once. It first appears in Entry_ID '
                            + str(seen_parameters[param_obj.name])
                            + ' and again in Entry_ID '
                            + str(entry_id)
                            + '. All parameters listed in the table must be unique.'
                        )
                    seen_parameters[param_obj.name] = entry_id

                    table_entry = parameter_table_entry(entry_id, param_obj)
                    store_parameter_table_entry(table_entry)
                    entry_id += 1

        # Store all dependencies
        self.dependencies.extend(dependencies)

        # Compute total number of component parameters (sum of all parameters across all entries)
        self.total_component_parameters = sum(len(entry.parameters) for entry in self.parameters.values())

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
        self.destinations = {}
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
            self.destinations[component_name] = indexes

        # Resolve all parameter component ids for each parameter in each entry:
        # Note: Parameter IDs are NOT set here - they will be accessed directly from
        # the parameter model at template render time, after the assembly has set them.
        for table_entry in self.parameters.values():
            for param in table_entry.parameters:
                param.component_id = self.destinations[param.component.instance_name][0]

        # Remove duplicate dependencies
        self.dependencies = list(set(self.dependencies))

    def _check_duplicate_parameters_across_tables(self):
        """
        Check that no duplicate parameters exist across multiple parameter tables
        in the assembly. This prevents the same parameter from being managed by
        multiple Parameters component instances.

        Note: This function is called each time a parameter table is loaded. Early tables
        may not see later tables yet, but eventually the last table to be loaded will see
        all other tables and perform a comprehensive check.
        """
        # Collect all OTHER parameter tables from the assembly's submodels that have been resolved
        other_table_parameters = {}  # maps parameter name -> (table_name, entry_id)

        for submodel_path, submodel in self.assembly.submodels.items():
            # Check if this submodel is a parameter_table (but not self)
            if isinstance(submodel, parameter_table) and submodel is not self:
                # Make sure the other table has been resolved
                if not submodel.parameter_table_resolved:
                    continue

                # Collect all parameters from this other table
                for table_entry in submodel.parameters.values():
                    for param in table_entry.parameters:
                        param_name = param.name
                        if param_name in other_table_parameters:
                            # This parameter already exists in another table!
                            prev_table_name, prev_entry_id = other_table_parameters[param_name]
                            raise ModelException(
                                f'Parameter "{param_name}" appears in multiple parameter tables. '
                                f'It is present in parameter table "{prev_table_name}" (Entry_ID {prev_entry_id}) '
                                f'and also in parameter table "{submodel.name}" (Entry_ID {table_entry.entry_id}). '
                                f'Each parameter can only be managed by one Parameters component instance.'
                            )
                        other_table_parameters[param_name] = (submodel.name, table_entry.entry_id)

        # Now check if any of OUR parameters conflict with those in other tables
        for table_entry in self.parameters.values():
            for param in table_entry.parameters:
                param_name = param.name
                if param_name in other_table_parameters:
                    prev_table_name, prev_entry_id = other_table_parameters[param_name]
                    raise ModelException(
                        f'Parameter "{param_name}" appears in multiple parameter tables. '
                        f'It is present in parameter table "{prev_table_name}" (Entry_ID {prev_entry_id}) '
                        f'and also in parameter table "{self.name}" (Entry_ID {table_entry.entry_id}). '
                        f'Each parameter can only be managed by one Parameters component instance.'
                    )

    @throw_exception_with_filename
    def set_assembly(self, assembly):
        """
        Public function to resolve all of the parameter ids, given
        an assembly model.
        """
        # Make sure an assembly is set by the base class implementation.
        super(parameter_table, self).set_assembly(assembly)

        # Resolve the parameter table now that we have an assembly object.
        self._resolve_parameter_table()

        # Check for duplicate parameters across multiple parameter tables
        self._check_duplicate_parameters_across_tables()
