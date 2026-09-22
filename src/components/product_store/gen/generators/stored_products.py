import os.path
from models import stored_products
from models import product_store_packets
from base_classes.generator_base import generator_base
from generators.basic import basic_generator, add_basic_generators_to_module
from generators.ided_suite import packet_templates
from util import redo_arg
from util import model_loader
from models.exceptions import ModelException


def load_stored_products_model(input_filename):
    """Helper which loads a stored products model, fully resolved by the assembly it is a part of."""
    p = stored_products.stored_products(input_filename)
    dirname, view_name, assembly_name, *ignore = redo_arg.split_model_filename(
        p.full_filename
    )
    assembly = model_loader.try_load_model_by_name(
        assembly_name, model_types="assembly"
    )
    if not assembly:
        raise ModelException(
            "Assembly submodel '"
            + p.model_name
            + "' could not find a model file for assembly '"
            + assembly_name
            + "'."
        )
    p.set_assembly(assembly)
    p.final()
    return p


class stored_products_gen(basic_generator):
    def __init__(self, template_filename, additional_template_dirs=[]):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dirs = [
            os.path.join(this_file_dir, ".." + os.sep + "templates")
        ] + additional_template_dirs
        basic_generator.__init__(
            self,
            model_class=stored_products.stored_products,
            template_filename=template_filename,
            template_dir=template_dirs,
        )

    def generate(self, input_filename):
        p = load_stored_products_model(input_filename)
        print(p.render(self.template, template_path=self.template_dir))

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )
        build_dir = self._get_default_build_dir()

        # Construct the filename:
        a = self.template_basename.rsplit("name", maxsplit=1)
        base_str = model_name + "_stored_products"
        if specific_name:
            base_str = base_str + "_" + specific_name
        output_fname = (base_str).join(a)
        return dirname + os.sep + build_dir + os.sep + output_fname


class stored_products_ads(stored_products_gen, generator_base):
    def __init__(self):
        stored_products_gen.__init__(self, template_filename="name.ads")


# Add all the basic generators for normal packets, except use the product_store_packets
# model. This allows us to build all the normal packet outputs like html, tex, etc. with
# the packet contents described by the stored products model.
add_basic_generators_to_module(
    product_store_packets.product_store_packets,
    packet_templates,
    module=globals(),
)
