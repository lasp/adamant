from models.exceptions import ModelException
from util import ada
from collections import OrderedDict
import os
from models.base import base
from models.memory_map import map_item


class register_map(base):
    """
    This is the object model for a register map. It extracts data from an
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the packed type object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        super(register_map, self).__init__(
            filename, os.environ["SCHEMAPATH"] + "/register_map.yaml"
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
        self.name = ada.formatType(os.path.basename(self.model_name))
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        if "with" in self.data and self.data["with"]:
            self.includes = list(self.data["with"])

        # Load the items and assign addresses:
        for item_data in self.data["items"]:
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
                # If the item is 4 bytes long then it requires volatile_full_access. Otherwise
                # it should just be volatile.
                if (
                    item.type.endswith("Register_T")
                    or item.type.endswith("Register_T_Le")
                ) and item.size == 4:
                    item.volatile_aspect = "Volatile_Full_Access"
                elif item.type.endswith("Register_T") or item.type.endswith(
                    "Register_T_Le"
                ):
                    item.volatile_aspect = "Volatile"
                elif item.type.endswith("Volatile_T") or item.type.endswith(
                    "Volatile_T_Le"
                ):
                    item.volatile_aspect = "Volatile"
                elif item.type.endswith("Atomic_T") or item.type.endswith(
                    "Atomic_T_Le"
                ):
                    item.volatile_aspect = "Atomic"
                else:
                    item.volatile_aspect = None

                # Add item to dictionary:
                unsorted_items[item.name] = item
            except ModelException as e:
                raise ModelException(
                    "Error encountered loading item: " + str(item) + " - " + str(e)
                )

        # Sort the items in the ordered dictionary:
        self.items = OrderedDict(
            sorted(unsorted_items.items(), key=lambda item: item[1].address)
        )

        # Set previous item in dictionary.
        prev_item = None
        for item in self.items.values():
            item.prev_item = prev_item
            prev_item = item

        # Ok check for overlap:
        for item in self.items.values():
            if item.prev_item:
                assert (
                    item.address >= item.prev_item.address
                ), "This should be sorted by logic above."
                prev_item_end_address = item.prev_item.address + item.prev_item.size - 1
                if (
                    item.address <= item.prev_item.address
                    or item.address <= prev_item_end_address
                ):
                    raise ModelException(
                        'Register map overlap encountered between items: "'
                        + str(item)
                        + '" and  "'
                        + str(item.prev_item)
                        + '" at address: 0x'
                        + str("%08X" % item.address)
                    )

        # Create some other useful attributes:
        self.num_items = len(self.items)
        self.type_includes = list(
            OrderedDict.fromkeys(
                [f.type_package for f in self.items.values() if f.type_package]
            )
        )
