from models.data_products import data_products, data_product
from models.exceptions import ModelException
from util import model_loader


class product_extractor_data_products(data_products):
    """
    This model is a specialized data product suite for the product extractor data products. This
    class inherits from the normal data products model but adds extra data products based on the user's model input.
    """
    def submodel_name(self):
        """This is just a "decorated" data product object, so make sure everyone treats it like that."""
        return "data_products"

    def set_assembly(self, assembly):
        # Make sure an assembly is set by the base class implementation.
        super(product_extractor_data_products, self).set_assembly(assembly)

        # Get the model for the downsampler to create data products for each item in the list
        extraction_list = self.component.init.get_parameter_value(
            "Data_Product_Extraction_List"
        )
        extraction_list_package = extraction_list.split(".")[0]
        extraction_list_model = model_loader.try_load_model_by_name(
            extraction_list_package, model_types="extracted_products"
        )
        if not extraction_list_model:
            raise ModelException(
                "Could not find extraction list model for the product extractor: "
                + str(extraction_list)
            )

        # Remove the dummy variable
        self.entities.pop("Dummy")

        # loop through the model to get all the data products we need to add
        for apid, products in extraction_list_model.apids.items():
            for data_product_item in products:
                # Check for the first time through to replace the dummy data product
                self.entities[data_product_item.name] = data_product(
                    name=data_product_item.name,
                    type=data_product_item.product_type
                    + "."
                    + data_product_item.product_endian,
                    description=data_product_item.description,
                    id=None,
                    suite=self,
                )
                dp = self.entities[data_product_item.name]

                # Let's also add the type to our suite and component's complex types:
                self.type_models.append(dp.type_model)
                self.component.complex_types[dp.type_model.name] = dp.type_model
