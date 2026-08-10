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
    _items_from_record
)
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


crc_obj = [None]


def _get_crc_size():
    # The store CRC is a Crc_16, which is 2 bytes (16 bits):
    return 16


class store_entry(object):
    def __init__(
        self,
        name,
        description=None,
        store_timestamp=False,
        restore_time=None,
        event_on_missing=True,
    ):
        if not name:
            raise ModelException(
                'Store data product list MUST specify a "name".'
            )

        split_name = name.split(".")
        if len(split_name) != 2:
            raise ModelException(
                'Store contains invalid data product name "'
                + name
                + '". Data product names should be of the format "Component.Data_Product_Name".'
            )
        self.name = ada.formatType(name)
        self.component_name = ada.formatVariable(split_name[0])
        self.data_product_name = ada.formatType(split_name[1])

        self.description = description
        self.store_timestamp = store_timestamp
        self.event_on_missing = event_on_missing

        # Default the restore_time based on the store_timestamp configuration. A
        # stored timestamp is stored precisely so that it can be restored, so it
        # is the default (and required, below) in that case. Otherwise the save
        # time is always available and is the most truthful default:
        if restore_time is None:
            restore_time = (
                "Use_Stored_Dp_Time" if store_timestamp else "Use_Save_Time"
            )
        self.restore_time = restore_time

        # Make sure the restore_time configuration is compatible with the
        # store_timestamp configuration, in both directions:
        if self.restore_time == "Use_Stored_Dp_Time" and not self.store_timestamp:
            raise ModelException(
                'Store entry "'
                + self.name
                + '" specifies a restore_time of "Use_Stored_Dp_Time" but does not specify '
                + '"store_timestamp: True". The stored data product time can only be used if it is stored.'
            )
        if self.store_timestamp and self.restore_time != "Use_Stored_Dp_Time":
            raise ModelException(
                'Store entry "'
                + self.name
                + '" specifies "store_timestamp: True" but a restore_time of "'
                + self.restore_time
                + '". A stored timestamp must be restored with "Use_Stored_Dp_Time", '
                + 'otherwise the stored timestamp would be silently ignored on restore.'
            )

        # Variables to be set during resolving of ids.
        self.component = None  # the component model
        self.data_product = None  # the data product model
        self.size = None  # in bits

    @classmethod
    @throw_exception_with_lineno
    def from_entry_data(cls, entry_data):
        name = entry_data["name"]

        description = None
        if "description" in entry_data:
            description = entry_data["description"]

        # Set store_timestamp, default False:
        store_timestamp = False
        if "store_timestamp" in entry_data and entry_data["store_timestamp"]:
            store_timestamp = True

        # Set restore_time. The default depends on store_timestamp and is
        # resolved in the constructor:
        restore_time = None
        if "restore_time" in entry_data:
            restore_time = entry_data["restore_time"]

        # Set event_on_missing, default True:
        event_on_missing = True
        if "event_on_missing" in entry_data:
            event_on_missing = entry_data["event_on_missing"]

        return cls(
            name=name,
            description=description,
            store_timestamp=store_timestamp,
            restore_time=restore_time,
            event_on_missing=event_on_missing,
        )


class store_packet(packet):
    """
    A specialized packet object which describes the contents of the product
    store dump packet. The items of this packet are derived from the store
    layout: the CRC, the save time (if configured), and each stored data
    product (preceded by its timestamp, if configured).
    """
    def __init__(self, name, store_model, id=None, suite=None):
        self.store_model = store_model
        super(store_packet, self).__init__(
            name,
            type=None,
            description="This packet contains the contents of the data product store.",
            id=id,
            suite=suite,
        )

    def create_item_list(self):
        from models.submodels.field import field

        # The store CRC comes first:
        crc_item = field(
            name="Store_Crc",
            type="Crc_16.Crc_16_Type",
            start_bit=0,
            start_field_number=0,
            format_string="U16",
            description="The CRC computed over the contents of the store at the last save.",
        )
        crc_item.flattened_description = (
            self.name + ".Store_Crc - The CRC computed over the contents of the store."
        )
        crc_item.full_name = self.name + ".Store_Crc"
        self.items.update({crc_item.full_name: crc_item})

        # Next comes the save time:
        items, ignore = _items_from_record(_get_time_obj())
        new_names = [
            (self.name + ".Save_Time." + name) for name in items.keys()
        ]
        self.items.update(OrderedDict(zip(new_names, items.values())))

        # Next each data product entry: its stored length byte, then its timestamp
        # (if configured), then its value:
        for entry in self.store_model.entries:
            length_item = field(
                name="Length",
                type="Data_Product_Types.Data_Product_Buffer_Length_Type",
                start_bit=0,
                start_field_number=0,
                format_string="U8",
                description="The stored length of the data product. A length of zero means the data product has never been saved.",
            )
            length_item.flattened_description = (
                self.name + "." + entry.name
                + ".Length - The stored length of the data product (zero means never saved)."
            )
            length_item.full_name = self.name + "." + entry.name + ".Length"
            self.items.update({length_item.full_name: length_item})
            if entry.store_timestamp:
                items, ignore = _items_from_record(_get_time_obj())
                new_names = [
                    (self.name + "." + entry.name + ".Time." + name)
                    for name in items.keys()
                ]
                self.items.update(OrderedDict(zip(new_names, items.values())))
            items, ignore = items_list_from_ided_entity(entry.data_product)
            new_names = [
                (self.name + "." + entry.name + "." + name)
                for name in items.keys()
            ]
            self.items.update(OrderedDict(zip(new_names, items.values())))

    def load_type_ranges(self):
        """
        Override this method so we can load the type ranges for the data products
        that make up this packet:
        """
        for entry in self.store_model.entries:
            try:
                entry.data_product.type_model.load_type_ranges()
            except AttributeError:
                pass


class stored_products(assembly_submodel):
    """
    This is the object model for a data product store. It extracts data from
    an input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the store object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(stored_products, self).__init__(
            filename, schema_dir + "/stored_products.yaml"
        )

    def load(self):
        """Load store specific data structures with information from YAML file."""
        # Load the base class model:
        super(stored_products, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.save_time = "Current_Time"
        self.entries = []
        self.store_size = None  # in bytes, set during resolution

        # Populate the object with the contents of the file data:
        self.name = ada.formatVariable(self.model_name) + "_Stored_Products"
        if self.specific_name:
            self.name = self.name + "_" + ada.formatVariable(self.specific_name)
        if "description" in self.data:
            self.description = self.data["description"]
        if "save_time" in self.data:
            self.save_time = self.data["save_time"]

        # Load the entries:
        entry_names = []
        for entry_data in self.data["data_products"]:
            entry = store_entry.from_entry_data(entry_data)
            entry.lineno = entry_data.lc.line

            # Make sure the entry is not a duplicate:
            if entry.name in entry_names:
                raise ModelException(
                    'duplicate data product found in store: "' + entry.name + '"',
                    lineno=entry.lineno,
                )
            entry_names.append(entry.name)
            self.entries.append(entry)

    def _resolve_data_products(self, assembly):
        # The assembly should be loaded first. For each entry, resolve the
        # data product model and size, and compute the total store size. The
        # store header always holds the CRC followed by the save time. Each
        # entry then holds a one byte stored length (zero means never saved),
        # followed by the timestamp (if configured), followed by the value:
        store_size = _get_crc_size() + _get_time_obj().size  # in bits
        self.type_packages = []

        for entry in self.entries:
            # Make sure the component for the data product exists:
            comp = assembly.get_component_with_name(entry.component_name)
            if not comp:
                raise ModelException(
                    'Store contains data product "'
                    + entry.name
                    + '", but the component "'
                    + entry.component_name
                    + '" does not exist in the assembly "'
                    + assembly.name
                    + '".',
                    lineno=entry.lineno,
                )
            entry.component = comp

            # Make sure the component contains data products:
            if not entry.component.data_products:
                raise ModelException(
                    'Store contains data product "'
                    + entry.name
                    + '", but the component "'
                    + entry.component_name
                    + '" does not have any data products.',
                    lineno=entry.lineno,
                )

            # Make sure the component contains the specified data product name:
            if entry.data_product_name not in entry.component.data_products.names():
                raise ModelException(
                    'Store contains data product "'
                    + entry.name
                    + '", but the component "'
                    + entry.component.instance_name
                    + '" does not contain a data product of that name, it only has data products '
                    + 'of the following names: '
                    + str(list(entry.component.data_products.names())),
                    lineno=entry.lineno,
                )

            # Set the data product:
            entry.data_product = entry.component.data_products.get_with_name(
                entry.data_product_name
            )
            self.dependencies.extend(
                [entry.component.data_products.full_filename] +
                entry.component.data_products.get_dependencies()
            )

            # Set the size:
            if entry.data_product.type_model is None:
                raise ModelException(
                    'Data product "'
                    + entry.name
                    + '" in store has type "'
                    + str(entry.data_product.type)
                    + '" whose model could not be found in the build path.',
                    lineno=entry.lineno,
                )
            entry.size = entry.data_product.type_model.size  # in bits

            # Make sure the data product type is "always valid", meaning that no
            # bit representation of the type can fail validation. The store zeroes
            # the slots of data products that cannot be fetched on save, and MRAM
            # contents could theoretically be corrupted without violating the CRC
            # (i.e. a matching recomputation). Requiring always valid types
            # guarantees a restore can never inject a data product whose use
            # downstream raises a constraint error:
            if not entry.data_product.type_model.is_always_valid():
                raise ModelException(
                    'Data product "'
                    + entry.name
                    + '" in store has type "'
                    + str(entry.data_product.type)
                    + '" which is not "always valid", meaning some bit representations '
                    + 'of the type fail validation. Only always valid types may be '
                    + 'included in a product store, since restored values must never '
                    + 'be able to cause a constraint error.',
                    lineno=entry.lineno,
                )

            # Save the type package name for the template, which emits a compile
            # time check that each type is always valid:
            if entry.data_product.type_package not in self.type_packages:
                self.type_packages.append(entry.data_product.type_package)

            # Calculate the total store size, including the entry's length byte:
            store_size += 8 + entry.size
            if entry.store_timestamp:
                store_size += _get_time_obj().size

        # Check that the store will fit inside of a packet data type:
        if store_size > _get_packet_buffer_size():
            raise ModelException(
                'Store "'
                + self.name
                + '" has size '
                + str(store_size)
                + " bits, which is larger than the buffer size of a Packet.T, which is "
                + str(_get_packet_buffer_size())
                + " bits. The store must fit within a single packet."
            )

        # Set the store size in bytes:
        self.store_size = int(store_size / 8)

        self.dependencies = list(dict.fromkeys(self.dependencies))

    def final(self):
        """We use the final function to resolve the data product entries against the assembly."""
        self._resolve_data_products(self.assembly)
