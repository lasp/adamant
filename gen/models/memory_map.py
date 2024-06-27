from models.exceptions import ModelException, throw_exception_with_lineno
from util import ada
from collections import OrderedDict
import os
from models.base import base
from models.submodels.variable import variable


class map_item(variable):
    """A memory map item. This is basically a variable, but with a few added fields."""
    def __init__(self, name, type, address=None, description=None):
        self.address = address
        self.prev_item = None
        super(map_item, self).__init__(
            name=name,
            type=type,
            description=description,
            value=None,
            default_value=None,
        )

        # Make sure the item is a packed type.
        if not (self.is_packed_type and self.type_model):
            raise ModelException(
                "Type '"
                + str(type)
                + "' is not allowed for memory map. Items must be packed types."
            )

        self.size = int(self.type_model.size / 8)

    @classmethod
    @throw_exception_with_lineno
    def from_item_data(cls, item_data):
        name = item_data["name"]
        type = item_data["type"]
        description = None
        if "description" in item_data:
            description = item_data["description"]
        address = None
        if "address" in item_data:
            address = item_data["address"]
        return cls(name=name, type=type, address=address, description=description)


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

        # Load the items and assign addresses:
        addr = self.start_address
        prev_item = None
        for item_data in self.data["items"]:
            try:
                # Create item from yaml:
                item = map_item.from_item_data(item_data)
                # Make sure item has a unique name:
                if item.name in self.items:
                    raise ModelException(
                        "Item list cannot contain duplicate item name '"
                        + str(item.name)
                        + "'."
                    )
                # Determine this item's address.
                item.address = addr
                # Determine the address for the next item.
                addr = item.address + item.size
                # Store the previous item
                item.prev_item = prev_item
                prev_item = item
                # Add item to dictionary:
                self.items[item.name] = item
            except ModelException as e:
                raise ModelException(
                    "Error encountered loading item: " + str(item) + " - " + str(e)
                )
        self.num_items = len(self.items)
        self.type_includes = list(
            OrderedDict.fromkeys(
                [f.type_package for f in self.items.values() if f.type_package]
            )
        )
        last_item = self.items[next(reversed(self.items))]
        self.last_used_address = last_item.address + item.size - 1
        if self.last_used_address >= self.start_address + self.length - 1:
            raise ModelException(
                "Memory map exceeds the last memory address: "
                + str("0x%08X" % self.last_used_address)
                + " > "
                + str("0x%08X" % (self.start_address + self.length - 1))
            )
