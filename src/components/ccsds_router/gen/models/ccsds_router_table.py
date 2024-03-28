from util import ada
import os.path
from collections import OrderedDict
from models.exceptions import (
    ModelException,
    throw_exception_with_lineno,
    throw_exception_with_filename,
)
from models.assembly import assembly_submodel


class router_table_entry(object):
    def __init__(
        self, apid, destinations=[], description=None, sequence_check="No_Check"
    ):
        self.apid = apid
        self.destinations = list(
            OrderedDict.fromkeys(
                [ada.formatVariable(component_name) for component_name in destinations]
            )
        )
        self.destination_indexes = None  # To be filled in by ccsds_router_table class
        self.description = description
        self.sequence_check = ada.formatType(sequence_check)

    @classmethod
    @throw_exception_with_lineno
    def from_entry_data(cls, entry_data):
        apid = entry_data["apid"]
        destinations = entry_data["destinations"]

        description = None
        if "description" in entry_data:
            description = entry_data["description"]

        sequence_check = "No_Check"
        if "sequence_check" in entry_data:
            sequence_check = entry_data["sequence_check"]

        return cls(
            apid=apid,
            destinations=destinations,
            description=description,
            sequence_check=sequence_check,
        )


# This is the object model for a router table. It extracts data from a
# input file and stores the data as object member variables.
class ccsds_router_table(assembly_submodel):
    # Initialize the table object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(ccsds_router_table, self).__init__(
            filename, schema_dir + "/ccsds_router_table.yaml"
        )

    # Load command specific data structures with information from YAML file.
    def load(self):
        # Initialize members:
        self.name = None
        self.description = None
        self.ccsds_router_instance_name = None
        self.ccsds_router_instance_model = None
        self.destinations = OrderedDict()  # Maps destination name to indexes
        self.table = OrderedDict()  # Maps APID to list of destination indexes

        # Populate the object with the contents of the
        # file data:
        if self.specific_name:
            self.name = ada.formatType(self.specific_name)
        else:
            self.name = ada.formatType(self.model_name) + "_Ccsds_Router_Table"
        if "description" in self.data:
            self.description = self.data["description"]
        self.ccsds_router_instance_name = ada.formatVariable(
            self.data["ccsds_router_instance_name"]
        )

        # Load the table entries:
        for entry_data in self.data["table"]:
            entry = router_table_entry.from_entry_data(entry_data)

            # Make sure the apid is not a duplicate:
            if entry.apid in self.table:
                raise ModelException(
                    'duplicate APID found: "' + str(entry.apid) + '"',
                    lineno=entry_data.lc.line,
                )
            else:
                self.table[entry.apid] = entry

    # Public function to resolve all of the router destinations to the correct connector indexes.
    @throw_exception_with_filename
    def resolve_router_destinations(self, assm):
        # Verify that the ccsds router component instance that this router table is being constructed
        # for actually exists in the assembly and is a Ccsds_Router component type.
        self.ccsds_router_instance_model = assm.get_component_with_name(
            self.ccsds_router_instance_name
        )
        if not self.ccsds_router_instance_model:
            raise ModelException(
                'Component instance "'
                + self.ccsds_router_instance_name
                + '" does not exist in the assembly "'
                + assm.name
                + '", so a CCSDS Router Table cannot be built.'
            )
        if not self.ccsds_router_instance_model.name == "Ccsds_Router":
            raise ModelException(
                'Component instance "'
                + self.ccsds_router_instance_name
                + '" in assembly "'
                + assm.name
                + '" is not of type "Ccsds_Router" so a CCSDS Router Table cannot be built. It is instead of type "'
                + self.ccsds_router_instance_model.name
                + '".'
            )

        # Add model to dependencies:
        self.dependencies.append(
            self.ccsds_router_instance_model.full_filename
        )

        # Get the router's output connector:
        router_connector = self.ccsds_router_instance_model.connectors.of_name(
            "Ccsds_Space_Packet_T_Send"
        )
        # Get all the component names connected to that connector:
        connections = router_connector.get_connections()
        connected_components = {}
        for idx, c in enumerate(connections):
            if c is not None and c != "ignore":
                component_name = c.to_component.instance_name
                connector_name = c.to_connector.name

                try:
                    # Append the connected data. This component is already connected.
                    # If we are not already connected to this connector then add this connector, otherwise
                    # this is an error.
                    if (
                        connector_name
                        in connected_components[c.to_component.instance_name]
                    ):
                        raise ModelException(
                            'Destination component "'
                            + component_name
                            + '" is attached to "'
                            + self.ccsds_router_instance_name
                            + '" twice on connector "'
                            + connector_name
                            + '". This is not allowed. The ccsds router may only be connected to unique'
                            + ' destination connectors.'
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
                    # Insert the connector and index that the connector is attached to:
                    connected_components[component_name] = {connector_name: idx + 1}

        # Create a list of all unique destinations:
        all_destinations = []
        for entry in self.table.values():
            all_destinations += entry.destinations
        all_destinations = list(OrderedDict.fromkeys(all_destinations))

        # Create map of destination name to indexes that it corresponds to:
        for destination_name in all_destinations:
            # Parse the destination name:
            split_name = destination_name.split(".")
            component_name = split_name[0]
            try:
                connector_name = split_name[1]
            except IndexError:
                connector_name = None

            # Get the routing indexes:
            index = None
            if component_name.lower() == "ignore":
                pass
            elif component_name in connected_components:
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
                            + '" so a table cannot be produced that routes to that destination.'
                        )
                elif len(connectors) == 1:
                    index = list(connectors.values())[0]
                else:
                    raise ModelException(
                        'Destination component "'
                        + component_name
                        + '" is connected to "'
                        + self.ccsds_router_instance_name
                        + '" on more than one connector. You must specify which connector to connect on in the form: "'
                        + component_name
                        + '.Connector_Name".'
                    )
            else:
                raise ModelException(
                    'Destination component "'
                    + component_name
                    + '" is not connected to "'
                    + self.ccsds_router_instance_name
                    + '" so a table cannot be produced that routes to that destination.'
                )
            # Add destination map to dict:
            self.destinations[destination_name] = index

        # Resolve all table entries:
        for entry in self.table.values():
            entry.destination_indexes = []
            # Translate each destination component name into its indexes
            for destination_component in entry.destinations:
                entry.destination_indexes.append(
                    self.destinations[destination_component]
                )
            # Cleanup:
            entry.destination_indexes = [
                e for e in OrderedDict.fromkeys(entry.destination_indexes) if e is not None
            ]

        # Remove duplicate dependencies
        self.dependencies = list(set(self.dependencies))
