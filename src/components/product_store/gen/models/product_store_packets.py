from models.packets import packets
from models.exceptions import ModelException
from util import model_loader
from collections import OrderedDict
import os.path


class product_store_packets(packets):
    """
    This model is a specialized packet suite for the Product Store packet. This
    class inherits from the normal packets model but adds some extra functionality.
    In particular, the contents of the Stored_Products packet is derived from the
    stored products model provided to the component instance within the assembly.
    The packet contents get filled in in the "set_assembly" method.
    """
    def submodel_name(self):
        """This is just a "decorated" packet object, so make sure everyone treats it like that."""
        return "packets"

    def set_assembly(
        self, assembly
    ):  # Make sure an assembly is set by the base class implementation.
        # Set assembly:
        self.assembly = assembly

        # Get the model for the product store so that we can describe the packet
        # contents. First get the package name from the discriminant:
        configs = self.component.discriminant.get_parameter_value("Store_Description")
        product_store_package = configs.split(".")[0]
        # Based on the package name figure out the model name:
        split_package = product_store_package.split("_Stored_Products")
        product_store_model_name = split_package[0]
        specific_name = None
        if len(split_package) > 1 and split_package[1]:
            specific_name = split_package[1][1:]

        # Get the model file paths:
        model_paths = model_loader.get_model_file_paths(
            product_store_model_name, model_types="stored_products"
        )
        if not model_paths:
            raise ModelException(
                "Could not find model for stored_products model: "
                + product_store_model_name
            )

        # Figure out which path this discriminant argument is referring to:
        model_path = None
        if specific_name:
            for p in model_paths:
                sp = os.path.basename(p).split(".")
                if len(sp) > 3 and sp[0].lower() == specific_name.lower():
                    model_path = p
                    break
        else:
            for p in model_paths:
                sp = os.path.basename(p).split(".")
                if len(sp) == 3:
                    model_path = p
                    break

        # Load the model from the path:
        self.product_store_model = model_loader.load_model(model_path)

        # Provide the assembly to the product store model:
        self.product_store_model.set_assembly(assembly)

        # Replace the placeholder packet contents with a packet described by
        # the store model. The packet keeps the standard name and (assembly
        # assigned) identifier of the placeholder found in the component's
        # product_store_packets.yaml.
        from models import stored_products

        new_entities = OrderedDict()
        for name, entity in self.entities.items():
            new_packet = stored_products.store_packet(
                name=entity.name,
                store_model=self.product_store_model,
                id=entity.id,
                suite=self,
            )
            new_entities[name] = new_packet
        self.entities = new_entities

        # Call the base class version:
        super(product_store_packets, self).set_component(self.component)
        super(product_store_packets, self).set_assembly(assembly)

    def final(self):
        # Call final on the product store model to finalize the entries, then
        # create the packet item lists:
        self.product_store_model.final()
        for entity in self.entities.values():
            entity.create_item_list()
