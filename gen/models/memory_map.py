from models.exceptions import ModelException, throw_exception_with_lineno
from util import ada
from collections import OrderedDict
import os
import zlib
from models.base import base
from models.submodels.variable import variable


class map_item(variable):
    """A memory map item. This is basically a variable, but with a few added fields."""
    def __init__(self, name, type, address=None, description=None, offset=None, size=None, alignment=None, packed_types_only=False):
        self.address = address
        self.offset = offset
        self.alignment = alignment
        self.prev_item = None
        super(map_item, self).__init__(
            name=name,
            type=type,
            description=description,
            value=None,
            default_value=None,
        )

        # Some maps only allow packed types.
        if packed_types_only and not self.is_packed_type:
            raise ModelException(
                "Type '"
                + str(type)
                + "' is not allowed. Items must be packed types."
            )

        # A type that looks like a packed type must have a model.
        if self.is_packed_type and not self.type_model:
            raise ModelException(
                "Type '"
                + str(type)
                + "' is not allowed for memory map. Could not find a model for this packed type."
            )

        # The size of the type in bytes. This is only known for packed types. The
        # size of any other type is checked against the item size by the compiler.
        self.type_size = None
        if self.is_packed_type:
            self.type_size = int(self.type_model.size / 8)

        # The size of the item is the number of bytes reserved for it in the map.
        # A packed type reserves exactly its own size unless a larger size is given.
        self.size_specified = size is not None
        if self.size_specified:
            if size < 1:
                raise ModelException(
                    "Item size must be at least 1 byte, but is: " + str(size)
                )
            if self.type_size is not None and size < self.type_size:
                raise ModelException(
                    "Item size of "
                    + str(size)
                    + " bytes is too small to hold type '"
                    + str(type)
                    + "' of size "
                    + str(self.type_size)
                    + " bytes."
                )
            self.size = size
        elif self.type_size is not None:
            self.size = self.type_size
        else:
            raise ModelException(
                "Type '"
                + str(type)
                + "' is not a packed type, so its size is not known to the model. A 'size' must be provided for this item."
            )

        if self.alignment is not None and self.alignment < 1:
            raise ModelException(
                "Item alignment must be at least 1 byte, but is: " + str(self.alignment)
            )
        if self.offset is not None and self.offset < 0:
            raise ModelException(
                "Item offset must not be negative, but is: " + str(self.offset)
            )

    @property
    def end_address(self):
        """The address of the last byte reserved for this item."""
        return self.address + self.size - 1

    @classmethod
    @throw_exception_with_lineno
    def from_item_data(cls, item_data, packed_types_only=False):
        name = item_data["name"]
        type = item_data["type"]
        description = None
        if "description" in item_data:
            description = item_data["description"]
        address = None
        if "address" in item_data:
            address = item_data["address"]
        offset = None
        if "offset" in item_data:
            offset = item_data["offset"]
        size = None
        if "size" in item_data:
            size = item_data["size"]
        alignment = None
        if "alignment" in item_data:
            alignment = item_data["alignment"]
        return cls(
            name=name,
            type=type,
            address=address,
            description=description,
            offset=offset,
            size=size,
            alignment=alignment,
            packed_types_only=packed_types_only,
        )


class memory_map(base):
    """
    This is the object model for a memory map. It extracts data from an
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the packed type object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        super(memory_map, self).__init__(
            filename, os.environ["SCHEMAPATH"] + "/memory_map.yaml"
        )

    def load(self):
        """Load record specific data structures with information from YAML file."""
        # Initialize object members:
        self.name = None
        self.description = None
        self.preamble = None
        self.items = OrderedDict()
        unsorted_items = OrderedDict()
        self.includes = []

        # Populate the object with the contents of the
        # file data:
        self.start_address = self.data["start_address"]
        self.length = self.data["length"]
        self.name = ada.formatType(os.path.basename(self.model_name))
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        if "with" in self.data and self.data["with"]:
            self.includes = list(self.data["with"])
        self.alignment = 1
        if "alignment" in self.data:
            self.alignment = self.data["alignment"]
        if self.alignment < 1:
            raise ModelException(
                "Memory map alignment must be at least 1 byte, but is: "
                + str(self.alignment)
            )
        self.last_address = self.start_address + self.length - 1

        # Load the items and assign addresses. An item with an offset is placed
        # there. An item without one is placed at the first aligned address after
        # the item listed before it.
        next_free_address = self.start_address
        for item_data in self.data["items"]:
            item = None
            try:
                # Create item from yaml:
                item = map_item.from_item_data(item_data)
                # Make sure item has a unique name:
                if item.name in unsorted_items:
                    raise ModelException(
                        "Item list cannot contain duplicate item name '"
                        + str(item.name)
                        + "'."
                    )
                # Determine this item's address.
                alignment = item.alignment if item.alignment is not None else self.alignment
                if item.offset is not None:
                    item.address = self.start_address + item.offset
                    if item.address % alignment != 0:
                        raise ModelException(
                            "Item at address "
                            + str("0x%08X" % item.address)
                            + " is not aligned to "
                            + str(alignment)
                            + " bytes."
                        )
                else:
                    item.address = -(-next_free_address // alignment) * alignment
                item.offset = item.address - self.start_address
                item.alignment = alignment
                # Make sure the item fits in the memory map.
                if item.end_address > self.last_address:
                    raise ModelException(
                        "Memory map exceeds the last memory address: "
                        + str("0x%08X" % item.end_address)
                        + " > "
                        + str("0x%08X" % self.last_address)
                    )
                # Determine the first free address after this item.
                next_free_address = item.end_address + 1
                # Add item to dictionary:
                unsorted_items[item.name] = item
            except ModelException as e:
                raise ModelException(
                    "Error encountered loading item: "
                    + str(item if item is not None else item_data["name"])
                    + " - "
                    + str(e)
                )

        # Sort the items in the ordered dictionary by address:
        self.items = OrderedDict(
            sorted(unsorted_items.items(), key=lambda item: item[1].address)
        )

        # Store the previous item and check for overlap:
        prev_item = None
        for item in self.items.values():
            item.prev_item = prev_item
            if prev_item and item.address <= prev_item.end_address:
                raise ModelException(
                    'Memory map overlap encountered between items: "'
                    + str(prev_item.name)
                    + '" ('
                    + str("0x%08X" % prev_item.address)
                    + " - "
                    + str("0x%08X" % prev_item.end_address)
                    + ') and "'
                    + str(item.name)
                    + '" ('
                    + str("0x%08X" % item.address)
                    + " - "
                    + str("0x%08X" % item.end_address)
                    + ")."
                )
            prev_item = item

        self.num_items = len(self.items)
        self.type_includes = list(
            OrderedDict.fromkeys(
                [
                    f.type_package
                    for f in self.items.values()
                    if f.is_packed_type and f.type_package
                ]
            )
        )
        self.last_used_address = max(
            item.end_address for item in self.items.values()
        )
        self.layout_id = self._calculate_layout_id()

    def _calculate_layout_id(self):
        """
        Calculate an identifier for the layout of this memory map. The identifier
        changes when the region, or the name, address, size, or type of any item
        changes. For a packed type the fields of the type are included, so a change
        to the layout of the type itself also changes the identifier.
        """
        description = [str(self.start_address), str(self.length)]
        for item in self.items.values():
            description.append(
                ":".join([item.name, str(item.address), str(item.size), item.type])
            )
            if item.is_packed_type:
                for a_field in item.type_model.flatten():
                    description.append(
                        ":".join([a_field.name, str(a_field.type), str(a_field.size)])
                    )
        return zlib.crc32("\n".join(description).encode("utf-8")) & 0xFFFFFFFF
