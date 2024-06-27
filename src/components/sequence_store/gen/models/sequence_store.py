from models.base import base
from util import ada
import os.path
from util import model_loader
from models.exceptions import ModelException

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
            return int(fld.size / 8)
    assert False, "No field 'Buffer' found in Packet.T type"


# Fetch the sequence store slot header size from the assembly, and save the result internally
# in case it is asked for again.
header_obj = [None]


def _get_slot_header_size():
    if header_obj[0] is None:
        header_obj[0] = model_loader.try_load_model_by_name(
            "Sequence_Store_Slot_Header", model_types="record"
        )
    if not header_obj[0]:
        raise ModelException(
            "Could not load model for Sequence_Store_Slot_Header.T. This must be in the path."
        )
    return int(header_obj[0].size / 8)


# Fetch the sequence store slot summary size from the assembly, and save the result internally
# in case it is asked for again.
summary_obj = [None]


def _get_slot_summary_size():
    if summary_obj[0] is None:
        summary_obj[0] = model_loader.try_load_model_by_name(
            "Packed_Slot_Summary", model_types="record"
        )
    if not summary_obj[0]:
        raise ModelException(
            "Could not load model for Packed_Slot_Summary.T. This must be in the path."
        )
    return int(summary_obj[0].size / 8)


class slot(object):
    def __init__(self, number, address, length):
        self.includes = []
        self.number = number

        try:
            self.address = int(address)
        except ValueError:
            self.address = address
            if "." in self.address:
                self.includes.append(ada.getPackage(self.address))

        try:
            self.length = int(length)
            # Make sure length is large enough to hold at least a header.
            if self.length < _get_slot_header_size():
                raise ModelException(
                    "Slot "
                    + str(number)
                    + " has size "
                    + str(self.length)
                    + " bytes which is too small. It must be at least "
                    + str(_get_slot_header_size())
                    + " bytes in size."
                )
        except ValueError:
            self.length = ada.formatValue(length)
            if "." in self.length:
                self.includes.append(ada.getPackage(self.length))


class sequence_store(base):
    """
    This is the object model for a sequence store. It extracts data from a
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
        super(sequence_store, self).__init__(
            filename, schema_dir + "/sequence_store.yaml"
        )

    def load(self):
        """Load command specific data structures with information from YAML file."""
        # Load the base class model:
        super(sequence_store, self).load()

        # Initialize some class members:
        self.name = None
        self.description = None
        self.slots = []

        # Populate the object with the contents of the
        # file data:
        self.name = ada.formatVariable(self.model_name)
        if "description" in self.data:
            self.description = self.data["description"]

        # Load the slots:
        self.includes = []
        num = 0
        for s in self.data["slots"]:
            new_slot = slot(number=num, address=s["address"], length=s["length"])
            self.slots.append(new_slot)
            self.includes.extend(new_slot.includes)
            num += 1
        self.includes = list(set(self.includes))

        self.summary_size = len(self.slots) * _get_slot_summary_size()
        if self.summary_size > _get_packet_buffer_size():
            raise ModelException(
                "Slot summary ("
                + str(self.summary_size)
                + " bytes) is too large to fit in the buffer of a packet ("
                + str(_get_packet_buffer_size())
                + " bytes)."
            )
