import os.path
from models import ccsds_downsampler_filters
from models import downsampler_data_products
from base_classes.generator_base import generator_base
from generators.basic import basic_generator, add_basic_generators_to_module
from generators.ided_suite import data_product_templates

add_basic_generators_to_module(
    downsampler_data_products.downsampler_data_products,
    data_product_templates,
    module=globals(),
)


class ccsds_filter_products_ads(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=ccsds_downsampler_filters.ccsds_downsampler_filters,
            template_filename="name.ads",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, component_name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (component_name).join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        p = ccsds_downsampler_filters.ccsds_downsampler_filters(input_filename)
        print(p.render(self.template, template_path=self.template_dir))
