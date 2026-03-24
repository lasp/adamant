from util import ada
import os.path
from collections import OrderedDict
from models.exceptions import (
    ModelException,
    throw_exception_with_lineno,
    throw_exception_with_filename,
)
from models.assembly import assembly_submodel


class destination_entry(object):
    def __init__(self, component_name, load_from=False):
        self.component_name = ada.formatVariable(component_name)
        self.load_from = load_from
        self.connector_index = None  # Filled in by resolve_router_destinations


class router_table_entry(object):
    def __init__(self, table_id, destinations=None, description=None):
        self.table_id = table_id
        # Preserve the original table_id as specified in YAML (before name resolution):
        self.original_table_id = table_id
        self.destinations = destinations if destinations else []
        self.description = description
        # Resolved destination entries with connector indexes and load_from flags.
        # Filled in by resolve_router_destinations.
        self.resolved_destinations = None

    @classmethod
    @throw_exception_with_lineno
    def from_entry_data(cls, entry_data):
        table_id = entry_data["table_id"]

        # Parse destinations:
        destinations = []
        for dest_data in entry_data["destinations"]:
            component_name = dest_data["component_name"]
            load_from = False
            if "load_from" in dest_data:
                load_from = dest_data["load_from"]
            destinations.append(
                destination_entry(
                    component_name=component_name,
                    load_from=load_from,
                )
            )

        if not destinations:
            raise ModelException(
                "At least one destination is required for table_id "
                + str(table_id)
                + "."
            )

        # Validate no duplicate destination component names:
        seen_names = []
        for dest in destinations:
            if dest.component_name in seen_names:
                raise ModelException(
                    'Duplicate destination "'
                    + dest.component_name
                    + '" found for table_id '
                    + str(table_id)
                    + ". Each destination must be unique within a table entry."
                )
            seen_names.append(dest.component_name)

        # Validate that at most one destination has load_from = True:
        load_from_count = sum(1 for d in destinations if d.load_from)
        if load_from_count > 1:
            raise ModelException(
                "At most one destination per table entry may have load_from "
                "set to True, but "
                + str(load_from_count)
                + " were found for table_id "
                + str(table_id)
                + "."
            )

        description = None
        if "description" in entry_data:
            description = entry_data["description"]

        return cls(
            table_id=table_id,
            destinations=destinations,
            description=description,
        )


class parameter_table_router_table(assembly_submodel):
    """
    This is the object model for a parameter table router table. It extracts
    data from an input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the table object, ingest data, and check it by
        calling the base class init function.
        """
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(parameter_table_router_table, self).__init__(
            filename, schema_dir + "/parameter_table_router_table.yaml"
        )

    def load(self):
        """Load table specific data structures with information from YAML file."""
        # Initialize members:
        self.name = None
        self.description = None
        self.parameter_table_router_instance_name = None
        self.parameter_table_router_instance_model = None
        self.destinations = OrderedDict()  # Maps destination name to connector index
        self.table = OrderedDict()  # Maps table_id to router_table_entry

        # Populate the object with the contents of the file data:
        if self.specific_name:
            self.name = ada.formatType(self.specific_name)
        else:
            self.name = (
                ada.formatType(self.model_name)
                + "_Parameter_Table_Router_Table"
            )
        if "description" in self.data:
            self.description = self.data["description"]
        self.parameter_table_router_instance_name = ada.formatVariable(
            self.data["parameter_table_router_instance_name"]
        )

        # Load the table entries:
        for entry_data in self.data["table"]:
            entry = router_table_entry.from_entry_data(entry_data)

            # Make sure the table_id is not a duplicate:
            if entry.table_id in self.table:
                raise ModelException(
                    'Duplicate table_id found: "' + str(entry.table_id) + '"',
                    lineno=entry_data.lc.line,
                )
            else:
                self.table[entry.table_id] = entry

    def _resolve_table_ids(self, assm):
        """Resolve string table_id values to numeric IDs by looking up parameter_table submodels."""
        from models.parameter_table import parameter_table

        # Build a map of parameter table name -> numeric table_id from the assembly's submodels:
        parameter_table_map = {}
        for _, submodel in assm.submodels.items():
            if isinstance(submodel, parameter_table) and submodel.table_id is not None:
                parameter_table_map[submodel.name] = submodel.table_id

        # Resolve string table_ids:
        resolved_table = OrderedDict()
        for table_id, entry in self.table.items():
            if isinstance(table_id, str):
                # Format the name the same way parameter_table models do:
                formatted_name = ada.formatVariable(table_id)
                if formatted_name not in parameter_table_map:
                    raise ModelException(
                        'table_id "'
                        + str(table_id)
                        + '" does not match any parameter table model in the '
                        + 'assembly "'
                        + assm.name
                        + '". Available parameter tables: '
                        + str(list(parameter_table_map.keys()))
                        + "."
                    )
                numeric_id = parameter_table_map[formatted_name]
                entry.table_id = numeric_id
                # Check for collision with an already-resolved ID:
                if numeric_id in resolved_table:
                    raise ModelException(
                        'table_id "'
                        + str(table_id)
                        + '" resolves to numeric ID '
                        + str(numeric_id)
                        + " which conflicts with another entry in the table."
                    )
                resolved_table[numeric_id] = entry
            else:
                if table_id in resolved_table:
                    raise ModelException(
                        'Duplicate table_id found after resolution: "'
                        + str(table_id)
                        + '".'
                    )
                resolved_table[table_id] = entry
        self.table = resolved_table

    @throw_exception_with_filename
    def resolve_router_destinations(self, assm):
        """Public function to resolve all of the router destinations to the correct connector indexes."""
        # Resolve any string table_id values to numeric IDs first:
        self._resolve_table_ids(assm)

        # Verify that the parameter table router component instance exists in the assembly
        # and is the correct component type.
        self.parameter_table_router_instance_model = (
            assm.get_component_with_name(
                self.parameter_table_router_instance_name
            )
        )
        if not self.parameter_table_router_instance_model:
            raise ModelException(
                'Component instance "'
                + self.parameter_table_router_instance_name
                + '" does not exist in the assembly "'
                + assm.name
                + '", so a Parameter Table Router Table cannot be built.'
            )
        if (
            not self.parameter_table_router_instance_model.name
            == "Ccsds_Parameter_Table_Router"
        ):
            raise ModelException(
                'Component instance "'
                + self.parameter_table_router_instance_name
                + '" in assembly "'
                + assm.name
                + '" is not of type "Ccsds_Parameter_Table_Router" so a '
                "Parameter Table Router Table cannot be built. It is instead "
                'of type "'
                + self.parameter_table_router_instance_model.name
                + '".'
            )

        # Add model to dependencies:
        self.dependencies.append(
            self.parameter_table_router_instance_model.full_filename
        )

        # Get the router's arrayed output connector:
        router_connector = (
            self.parameter_table_router_instance_model.connectors.of_name(
                "Parameters_Memory_Region_T_Send"
            )
        )
        # Get all the component names connected to that connector:
        connections = router_connector.get_connections()
        connected_components = {}
        for idx, c in enumerate(connections):
            if c is not None and c != "ignore":
                component_name = c.to_component.instance_name
                connector_name = c.to_connector.name

                try:
                    if (
                        connector_name
                        in connected_components[c.to_component.instance_name]
                    ):
                        raise ModelException(
                            'Destination component "'
                            + component_name
                            + '" is attached to "'
                            + self.parameter_table_router_instance_name
                            + '" twice on connector "'
                            + connector_name
                            + '". This is not allowed.'
                        )
                    else:
                        connected_components[c.to_component.instance_name][
                            connector_name
                        ] = (idx + 1)

                        # Add model to dependencies:
                        self.dependencies.append(
                            c.to_component.full_filename
                        )
                except KeyError:
                    connected_components[component_name] = {
                        connector_name: idx + 1
                    }

        # Create a list of all unique destination component names:
        all_destination_names = []
        for entry in self.table.values():
            for dest in entry.destinations:
                if dest.component_name not in all_destination_names:
                    all_destination_names.append(dest.component_name)

        # Create map of destination name to connector index:
        for destination_name in all_destination_names:
            split_name = destination_name.split(".")
            component_name = split_name[0]
            try:
                connector_name = split_name[1]
            except IndexError:
                connector_name = None

            index = None
            if component_name in connected_components:
                connectors = connected_components[component_name]
                if connector_name:
                    try:
                        index = connectors[connector_name]
                    except KeyError:
                        raise ModelException(
                            'Destination component "'
                            + component_name
                            + '" does not contain connector of name "'
                            + connector_name
                            + '" so a table cannot be produced that routes '
                            "to that destination."
                        )
                elif len(connectors) == 1:
                    index = list(connectors.values())[0]
                else:
                    raise ModelException(
                        'Destination component "'
                        + component_name
                        + '" is connected to "'
                        + self.parameter_table_router_instance_name
                        + '" on more than one connector. You must specify '
                        "which connector to connect on in the form: \""
                        + component_name
                        + '.Connector_Name".'
                    )
            else:
                raise ModelException(
                    'Destination component "'
                    + component_name
                    + '" is not connected to "'
                    + self.parameter_table_router_instance_name
                    + '" so a table cannot be produced that routes to '
                    "that destination."
                )
            self.destinations[destination_name] = index

        # Resolve all table entries - build resolved_destinations list:
        for entry in self.table.values():
            entry.resolved_destinations = []
            for dest in entry.destinations:
                connector_index = self.destinations[dest.component_name]
                if connector_index is not None:
                    entry.resolved_destinations.append(
                        {
                            "connector_index": connector_index,
                            "load_from": dest.load_from,
                        }
                    )

        # Remove duplicate dependencies:
        self.dependencies = list(set(self.dependencies))
