from util import ada
import os.path
from models.base import base
from models.exceptions import ModelException
from collections import OrderedDict
from util import model_loader


class data_product_entry(object):
    """Class to extract the details of each data product in the model list"""
    def __init__(self, product_data):
        self.name = ada.formatVariable(product_data["name"])
        self.apid = product_data["apid"]
        self.offset = product_data["offset"]
        self.time_type = product_data["time"]
        self.local_id = None
        # Check what product type we have so that we know the size of the product
        self.product_type = ada.formatVariable(product_data["type"].split(".")[0])
        self.product_endian = ada.formatVariable(product_data["type"].split(".")[1])
        if self.product_endian not in ["T", "T_Le"]:
            raise ModelException(
                "The type of the product must be a packed type: "
                + str(product_data["type"])
            )
        model_path = model_loader.try_load_model_by_name(
            self.product_type, model_types=["record"]
        )
        if not model_path:
            raise ModelException(
                "Could not find record model file: '" + self.product_type + "'."
            )

        # Type information for this type
        self.size = model_path.size
        self.length = int(self.size / 8)
        # Make sure the way we timestamp the data product is specified in the enumeration
        if self.time_type not in ["current_time", "packet_time"]:
            raise ModelException(
                "The time type must be current_time or packet_time: "
                + str(self.time_type)
            )

        self.description = None
        if "description" in product_data:
            self.description = product_data["description"]


class extracted_products(base):
    """
    This is the object model for the product extractor. It extracts data from a
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
        super(extracted_products, self).__init__(
            filename, schema_dir + "/extracted_products.yaml"
        )

    def load(self):
        # Load the base class model:
        super(extracted_products, self).load()

        # Initialize some class members:
        self.includes = []  # map from name to packet obj
        self.apids = (
            OrderedDict()
        )  # map of apids to track the various products in each packet

        # Name for the auto-generated files
        if self.specific_name:
            raise ModelException(
                "Specific name for the product_extractor is invalid: '"
                + self.specific_name
                + "'.\nDo not use a specific name for the product extractor list."
            )
        else:
            self.name = ada.formatVariable(self.model_name)

        if "description" in self.data:
            self.description = self.data["description"]

        # Find all the products from the input yaml file
        for product_data in self.data["data_products"]:
            the_products = data_product_entry(product_data)
            # Make sure that we get the list of types we need to include, but not to duplicate them.
            if the_products.product_type not in self.includes:
                self.includes.append(the_products.product_type)

            # Make sure there are no duplicate names for all data products regardless of apid
            for apid_num, product_list in self.apids.items():
                for item in product_list:
                    if the_products.name in item.name:
                        raise ModelException(
                            "Cannot have duplicate names in the product extractor: "
                            + str(the_products.name)
                        )

            if the_products.apid in self.apids:
                # Get the current list of products in the apid dictionary here and then append and save back
                self.apids[the_products.apid].append(the_products)

            else:
                self.apids[the_products.apid] = [the_products]

        # Set the local IDs for all the data products in order:
        the_id = 0
        for apid, products in self.apids.items():
            for data_product_item in products:
                data_product_item.local_id = the_id
                the_id += 1
