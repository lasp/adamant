from util import ada
import os.path
from models.base import base
from models.exceptions import ModelException
from collections import OrderedDict


class filter_entry(object):
    def __init__(self, packet):
        self.apid = packet["apid"]
        if self.apid > 2047:
            raise ModelException(
                "Downsampler List Apid larger than 11 bits: '" + self.apid + "'."
            )

        self.filter_factor = packet["filter_factor"]
        if self.filter_factor > 65535:
            raise ModelException(
                "Downsampler filter factor set to invalid value: '"
                + self.filter_factor
                + "'."
            )

        if "packet_name" in packet:
            self.name = ada.formatVariable(packet["packet_name"])
        else:
            self.name = ada.formatVariable("Apid_" + str(self.apid))

        if "description" in packet:
            self.description = packet["description"]
        else:
            self.description = ""


class ccsds_downsampler_filters(base):
    """
    This is the object model for the ccsds_downsampler. It extracts data from a
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        Initialize the products object, ingest data, and check it by
        calling the base class init function.
        """
        # Load the object from the file:
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(ccsds_downsampler_filters, self).__init__(
            filename, schema_dir + "/ccsds_downsampler_filters.yaml"
        )

    def load(self):
        # Load the base class model:
        super(ccsds_downsampler_filters, self).load()

        # Grab the name of the package and the description if it is included
        # self.package_name = self.name

        # Initialize some class members:
        self.name = None
        self.description = None
        self.product_list = []
        self.filter_products = OrderedDict()  # map from name to packet obj

        if self.specific_name:
            raise ModelException(
                "Specific name for the Downsampler is invalid: '"
                + self.specific_name
                + "'.\nDo not use a specific name for the downsampler apid list."
            )
        else:
            self.name = ada.formatVariable(self.model_name)

        if "description" in self.data:
            self.description = self.data["description"]

        # Get the information for each apid/filter_factor packet to be included in the downsampled list
        for packet in self.data["downsampled_packets"]:
            the_products = filter_entry(packet)
            if the_products.name not in self.filter_products:
                if the_products.apid not in self.filter_products:
                    # Get the current list of products in the dictionary here and make sure we dont have a duplicate
                    self.filter_products[the_products.apid] = the_products
                else:
                    raise ModelException(
                        "Duplicate apid found in Downsampler: '"
                        + the_products.apid
                        + "'."
                    )
            else:
                raise ModelException(
                    "Duplicate name found in Downsampler: '" + the_products.name + "'."
                )

        # Finally get the entire size of the list in bytes (2 per apid, 2 per filter_factor)
        self.size = len(self.filter_products) * 4
