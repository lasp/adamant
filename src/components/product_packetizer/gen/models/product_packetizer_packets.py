from models.packets import packets
from models.exceptions import ModelException
from util import model_loader
import os.path


class product_packetizer_packets(packets):
    """
    This model is a specialized packet suite for the CPU monitor packet. This
    class inherits from the normal packets model but adds some extra functionality
    needed to the CPU monitor packet. In particular, the CPU monitor packet's
    type is derived from data within the assembly that the packet is a part of. The
    type itself gets filled in in the "load_assembly" method.
    """
    def submodel_name(self):
        """This is just a "decorated" packet object, so make sure everyone treats it like that."""
        return "packets"

    def set_assembly(
        self, assembly
    ):  # Make sure an assembly is set by the base class implementation.
        # Set assembly:
        self.assembly = assembly

        # Get the model for the product packetizer so that we can create the packet list. First
        # get the package name:
        configs = self.component.discriminant.get_parameter_value("Packet_List")
        product_packetizer_package = configs.split(".")[0]
        # Based on the package name figure out the model name:
        split_package = product_packetizer_package.split("_Product_Packets")
        product_packetizer_model_name = split_package[0]
        specific_name = None
        if len(split_package) > 1:
            specific_name = split_package[1][1:]

        # Get the model file paths:
        model_paths = model_loader.get_model_file_paths(
            product_packetizer_model_name, model_types="product_packets"
        )
        if not model_paths:
            raise ModelException(
                "Could not model for product_packets model: "
                + product_packetizer_model_name
            )

        # Figure out which path this init argument is referring to:
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
        self.product_packetizer_model = model_loader.load_model(model_path)

        # Provide the assembly to the product packetizer model
        self.product_packetizer_model.set_assembly(assembly)

        # Set the component packets to the product packet model packets.
        self.entities = self.product_packetizer_model.packets
        self.ids = [e.id for e in self.entities.values() if e.id]

        # Call the base class version:
        super(product_packetizer_packets, self).set_component(self.component)
        super(product_packetizer_packets, self).set_assembly(assembly)

    def final(self):
        # Call final on the product packetizer to finalize the packets.
        self.product_packetizer_model.final()
