from models.data_products import data_products, data_product
from models.exceptions import ModelException
from util import model_loader


class downsampler_data_products(data_products):
    """
    This model is a specialized data product suite for the CCSDS Downsampler component.
    This class inherits from the normal data product model and adds data products for each
    item in the list that the downsampler could end up downsampling.
    """
    def submodel_name(self):
        """This is just a "decorated" packet object, so make sure everyone treats it like that."""
        return "data_products"

    def set_assembly(self, assembly):
        # Make sure an assembly is set by the base class implementation.
        super(downsampler_data_products, self).set_assembly(assembly)

        # Get the model for the downsampler to create data products for each item in the list
        filter_list = self.component.init.get_parameter_value("Downsample_List")
        filter_list_package = filter_list.split(".")[0]
        downsampler_list_model = model_loader.try_load_model_by_name(
            filter_list_package, model_types="ccsds_downsampler_filters"
        )

        # loop through the model to get all the data products we need to add
        if downsampler_list_model:
            for apid in downsampler_list_model.filter_products.values():
                self.entities[apid.name + "_Filter_Factor"] = data_product(
                    name=apid.name + "_Filter_Factor",
                    type="Packed_U16.T",
                    description=apid.description,
                    id=None,
                    suite=self,
                )
        else:
            raise ModelException(
                "Could not find model for Downsample_List '"
                + filter_list_package
                + "'."
            )
