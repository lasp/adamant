from util import ada
import os.path
from collections import OrderedDict
from models.exceptions import (
    ModelException,
    throw_exception_with_lineno
)
from models.packets import (
    packet,
    items_list_from_ided_entity,
    _items_from_record,
    packet_item
)
from models.submodels.field import field
from util import model_loader
from models.submodels.ided_suite import ided_entity
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


time_obj = [None]


def _get_time_obj():
    if time_obj[0] is None:
        time_obj[0] = model_loader.try_load_model_by_name(
            "Sys_Time", model_types="record"
        )
    if not time_obj[0]:
        raise ModelException(
            "Could not load model for Sys_Time.T. This must be in the path."
        )
    return time_obj[0]


class data_product_entry(object):
    def __init__(
        self,
        name=None,
        event_on_missing=False,
        use_timestamp=False,
        include_timestamp=False,
        pad_bytes=None,
    ):
        if pad_bytes and (
            name or event_on_missing or use_timestamp or include_timestamp
        ):
            raise ModelException(
                ('Packet data product list specifies "pad_bytes". If "pad_bytes" is specified'
                 'then [name, event_on_missing, use_timestamp, include_timestamp] may not be specified.')
            )

        if not name and not pad_bytes:
            raise ModelException(
                'Packet data product list MUST specify a "name" unless "pad_bytes" is specified.'
            )

        if name:
            split_name = name.split(".")
            if len(split_name) != 2:
                raise ModelException(
                    "Packet contains invalid data product name "
                    + name
                    + '". Data product names should be of the format "Component.Data_Product_Name".'
                )
            self.name = ada.formatType(name)
            self.component_name = ada.formatVariable(split_name[0])
            self.data_product_name = ada.formatType(split_name[1])
            self.pad_bytes = 0
        else:
            self.name = ""
            self.component_name = ""
            self.data_product_name = ""
            self.pad_bytes = pad_bytes

        self.event_on_missing = event_on_missing
        self.use_timestamp = use_timestamp
        self.include_timestamp = include_timestamp
        self.packet_period_item = False

        # Variables to be set during resolving of ids.
        self.component = None  # the component model
        self.data_product = None  # the data product model
        self.id = None  # the data product id
        self.size = None

    @classmethod
    @throw_exception_with_lineno
    def from_data_product_entry_data(cls, product_data):
        name = None
        if "name" in product_data and product_data["name"]:
            name = product_data["name"]

        # Set event_on_missing, default False:
        event_on_missing = False
        if "event_on_missing" in product_data and product_data["event_on_missing"]:
            event_on_missing = True

        # Set use_timestamp, default False:
        use_timestamp = False
        if "use_timestamp" in product_data and product_data["use_timestamp"]:
            use_timestamp = True

        # Set include_timestamp, default False:
        include_timestamp = False
        if "include_timestamp" in product_data and product_data["include_timestamp"]:
            include_timestamp = True

        # Set include_timestamp, default False:
        pad_bytes = None
        if "pad_bytes" in product_data and product_data["pad_bytes"]:
            pad_bytes = product_data["pad_bytes"]

        return cls(
            name=name,
            event_on_missing=event_on_missing,
            use_timestamp=use_timestamp,
            include_timestamp=include_timestamp,
            pad_bytes=pad_bytes,
        )


# Create a packet item that is slightly modified for the product packets from the standard
# packet items. This provides a different full_name and flattened_description method.
class product_packet_item(packet_item):
    def __init__(self, data, dp, packet):
        self.__dict__ = data
        self.dp = dp
        self.packet = packet

    @classmethod
    def from_packet_item(cls, packet_item, dp, packet):
        import copy

        data = copy.copy(packet_item.__dict__)
        return cls(data, dp, packet)

    @property
    def flattened_description(self):
        super_desc = super(product_packet_item, self).flattened_description
        prefix = (
            self.packet.name
            + "."
            + self.dp.name
            + " - "
            + str(self.dp.data_product.id)
            + (" (0x%04x)" % self.dp.data_product.id)
        )
        return prefix + " -" + "-".join(super_desc.split("-")[1:])

    @property
    def full_name(self):
        super_name = super(product_packet_item, self).full_name
        return self.packet.name + "." + super_name


class product_packet(packet):
    def __init__(
        self,
        name,
        id,
        period,
        data_products=[],
        description=None,
        offset="0",
        enabled=True,
        use_tick_timestamp=False,
        suite=None,
    ):
        self.period = period
        self.data_products = data_products
        self.offset = offset
        self.enabled = enabled
        self.use_tick_timestamp = use_tick_timestamp
        self.size = None  # To be set during id resolution

        # Make sure that only one data product in the packet (or zero) specify "use_timestamp"
        use_timestamp_count = 0
        for dp in data_products:
            if dp.use_timestamp:
                use_timestamp_count += 1
        if use_timestamp_count > 1:
            raise ModelException(
                "More than one data product in packet '"
                + str(name)
                + "' specifies 'use_timetamp'. Only one data product in a packet may specify this field."
            )
        if use_timestamp_count >= 1 and self.use_tick_timestamp:
            raise ModelException(
                "Packet '"
                + str(name)
                + "' specifies 'use_timetamp' for a data product and 'use_tick_timestamp'"
                + " as True. Both of these cannot be specified."
            )

        # Call the base class initialization
        super(product_packet, self).__init__(
            name, type=None, description=description, id=id, suite=suite
        )

    def create_item_list(self):
        # Create item list:
        pad_count = 0
        bit = 0
        for dp in self.data_products:
            if dp.name:
                if dp.include_timestamp:
                    items, bit = _items_from_record(_get_time_obj())
                    new_names = [
                        (self.name + "." + dp.name + "." + name)
                        for name in items.keys()
                    ]
                    # Add data product reference to item for usage later:
                    for item in items.values():
                        item.dp = dp
                    self.items.update(OrderedDict(zip(new_names, items.values())))
                # Get items list from the data product
                items, bit = items_list_from_ided_entity(dp.data_product, start_bit=bit)
                # Convert the packet items to product packet items, which have a bit different handling
                product_packet_items = [
                    product_packet_item.from_packet_item(item, dp, self)
                    for item in items.values()
                ]
                new_names = [item.full_name for name, item in items.items()]
                # Update the items dict:
                self.items.update(OrderedDict(zip(new_names, product_packet_items)))
            else:
                # Create fake field object to simulate pad bytes:
                item = field(
                    name=("Reserved_%02d" % pad_count),
                    type="Natural",
                    start_bit=0,
                    start_field_number=0,
                    format_string=(
                        "U8" + ("x" + str(dp.pad_bytes) if (dp.pad_bytes > 1) else "")
                    ),
                    description="Pad bytes.",
                )
                item.flattened_description = (
                    self.name + (".Reserved_%02d" % pad_count) + " - Unused pad bytes."
                )
                item.full_name = self.name + ".Reserved_%02d" % pad_count
                self.items.update({item.full_name: item})
                pad_count += 1

    # Override this method so we can load the type ranges for the data products
    # that make up this packet:
    def load_type_ranges(self):
        for dp_entry in self.data_products:
            try:
                dp_entry.data_product.type_model.load_type_ranges()
            except AttributeError:
                pass

    @classmethod
    @throw_exception_with_lineno
    def from_product_packet_data(cls, packet_data, suite=None):
        name = packet_data["name"]
        period = packet_data["period"]
        id = packet_data["id"]

        description = None
        if "description" in packet_data:
            description = packet_data["description"]

        enabled = True
        if "enabled" in packet_data and not packet_data["enabled"]:
            enabled = False

        offset = "0"
        if "offset" in packet_data:
            offset = packet_data["offset"]

        use_tick_timestamp = False
        if "use_tick_timestamp" in packet_data:
            use_tick_timestamp = packet_data["use_tick_timestamp"]

        data_products = []
        for dp_data in packet_data["data_products"]:
            data_products.append(
                data_product_entry.from_data_product_entry_data(dp_data)
            )

        return cls(
            name=name,
            id=id,
            period=period,
            data_products=data_products,
            description=description,
            offset=offset,
            enabled=enabled,
            use_tick_timestamp=use_tick_timestamp,
            suite=suite,
        )


# Used in class below...
class dummy:
    pass


# This is the object model for a packet suite. It extracts data from a
# input file and stores the data as object member variables.
class product_packets(assembly_submodel):
    # Initialize the packet object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(product_packets, self).__init__(
            filename, schema_dir + "/product_packets.yaml"
        )

    # Load command specific data structures with information from YAML file.
    def load(self):
        # Load the base class model:
        super(product_packets, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.includes = []
        self.packets = OrderedDict()  # map from name to packet obj
        self.packet_ids = []

        # Populate the object with the contents of the
        # file data:
        self.name = ada.formatVariable(self.model_name) + "_Product_Packets"
        if self.specific_name:
            self.name = self.name + "_" + ada.formatVariable(self.specific_name)
        if "description" in self.data:
            self.description = self.data["description"]
        if "with" in self.data:
            self.includes = self.data["with"]
        for include in self.includes:
            include = ada.formatType(include)
        self.includes = list(set(self.includes))

        # Load the packets:
        for packet_data in self.data["packets"]:
            packet = product_packet.from_product_packet_data(packet_data, self)
            packet.lineno = packet_data.lc.line

            # Make sure name is not a duplicate
            if packet.name not in self.packets:
                self.packets[packet.name] = packet
            else:
                raise ModelException(
                    'duplicate packet name found: "' + packet.name + '"',
                    lineno=packet.lineno,
                )

            # Make sure id is not a duplicate:
            if packet.id and (packet.id not in self.packet_ids):
                self.packet_ids.append(packet.id)
            else:
                raise ModelException(
                    'packet "'
                    + packet.name
                    + '" contains a duplicate packet id "'
                    + str(packet.id)
                    + '". All packet ids must be unique.',
                    lineno=packet.lineno,
                )

    def _resolve_data_product_size(self, assembly):
        # The assembly should be loaded first:
        # For each packet figure out the data product ids and packet size:
        for pkt in self.packets.values():
            packet_size = 0

            for dp in pkt.data_products:
                if dp.name:
                    # Make sure the component for the data product exists:
                    comp = assembly.get_component_with_name(dp.component_name)
                    if not comp:
                        raise ModelException(
                            'Packet "'
                            + pkt.name
                            + '" contains data product "'
                            + dp.name
                            + '", but the component "'
                            + dp.component_name
                            + '" does not exist in the assembly "'
                            + assembly.name
                            + '".',
                            lineno=pkt.lineno,
                        )
                    else:
                        # Save the component model:
                        dp.component = comp

                    # The product packetizer allows for a few "special" data product items to be issued
                    # based on information the product packetizer has inside of itself. If the component
                    # is itself a product packetizer, then we can consider this as one of those special
                    # items.
                    if dp.component.name == "Product_Packetizer":
                        # OK this could be a special item. See if it is one of the special items we know how to handle.
                        if dp.data_product_name.endswith("_Period"):
                            packet_name = dp.data_product_name.split("_Period")[0]
                            if packet_name in self.packets.keys():
                                # Ok this special packet checks out. Set the necessary details so the component
                                # knows how to deal with it. Create a dummy data product with the desired information.
                                dp.data_product = ided_entity(
                                    name=dp.data_product_name,
                                    type="Packed_Natural.T",
                                    description="Special product packetizer item which reports the current packet"
                                    + " period of the " + packet_name
                                    + " packet.",  # Index of packet in list for lookup by component (1-indexed)
                                    id=list(self.packets.keys()).index(packet_name) + 1,
                                    suite=dp.component.data_products,
                                )
                                dp.packet_period_item = True
                                # The product packetizer does not have a data product suite, but we create
                                # a dummy object with some internal properties to help out some templates
                                # downstream.
                                dp.data_product.suite = dummy()
                                dp.data_product.suite.component = dp.component
                            else:
                                raise ModelException(
                                    'Packet "'
                                    + pkt.name
                                    + '" contains special item "'
                                    + dp.name
                                    + '" but packet "'
                                    + packet_name
                                    + '" does not exist. Options: '
                                    + str(list(self.packets.keys())),
                                    lineno=pkt.lineno,
                                )
                        else:
                            raise ModelException(
                                'Packet "'
                                + pkt.name
                                + '" contains special item "'
                                + dp.name
                                + '" but only items ending in "_Period" are supported.',
                                lineno=pkt.lineno,
                            )

                    # Normal handling of the data product:
                    else:
                        # Normal handling of the data product:
                        # Make sure the component contains data products:
                        if not dp.component.data_products:
                            raise ModelException(
                                'Packet "'
                                + pkt.name
                                + '" contains data product "'
                                + dp.name
                                + '", but the component "'
                                + dp.component_name
                                + '" does not have any data products.',
                                lineno=pkt.lineno,
                            )

                        # Make sure the component contains the specified data product name:
                        if (
                            dp.data_product_name
                            not in dp.component.data_products.names()
                        ):
                            raise ModelException(
                                'Packet "'
                                + pkt.name
                                + '" contains data product "'
                                + dp.name
                                + '", but the component "'
                                + dp.component.instance_name
                                + '" does not contain a data product of that name, it only has data products '
                                + 'of the following names: '
                                + str(list(dp.component.data_products.names())),
                                lineno=pkt.lineno,
                            )

                        # Set the data product:
                        dp.data_product = dp.component.data_products.get_with_name(
                            dp.data_product_name
                        )
                        self.dependencies.extend(
                            [dp.component.data_products.full_filename] +
                            dp.component.data_products.get_dependencies()
                        )

                    # Set the size:
                    dp.size = dp.data_product.type_model.size  # in bits
                else:
                    dp.size = dp.pad_bytes * 8

                # Calculate the total length:
                packet_size += dp.size
                if dp.include_timestamp:
                    packet_size += _get_time_obj().size

            # Check that length will fit inside of a packet data type:
            if packet_size > _get_packet_buffer_size():
                raise ModelException(
                    'Packet "'
                    + pkt.name
                    + '" has size '
                    + str(packet_size)
                    + " bits, which is larger than the buffer size of a Packet.T, which is "
                    + str(_get_packet_buffer_size())
                    + " bits.",
                    lineno=pkt.lineno,
                )

            # Set the packet size
            pkt.size = packet_size

        self.dependencies = list(set(self.dependencies))

    # We use the final function to create the item list and resolve the data product IDS.
    def final(self):
        # Resolve the data products sizes now. We cannot resolve ids yet,
        # because at the time that this function is called, the assembly has
        # not yet set the ids.
        self._resolve_data_product_size(self.assembly)

        # Create the item list for each packet:
        for pkt in self.packets.values():
            # Create the item list for the packet:
            pkt.create_item_list()
