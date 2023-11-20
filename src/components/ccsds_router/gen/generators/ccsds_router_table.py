import os.path
from models import assembly
from models import ccsds_router_table
from base_classes.generator_base import generator_base
from util import error
from util import model_loader
from generators.basic import basic_generator


class ccsds_router_table_ads(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=ccsds_router_table.ccsds_router_table,
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
            output_filename = (component_name + "_ccsds_router_table").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        assembly_model_name = model_loader.get_model_file_path(
            assembly_name, model_types="assembly"
        )
        if not assembly_model_name:
            error.error_print(
                "Could not find a model file for assembly '" + assembly_name + "'."
            )
            error.abort()
        a = assembly.assembly(assembly_model_name)
        r = ccsds_router_table.ccsds_router_table(input_filename)
        r.resolve_router_destinations(a)
        print(r.render(self.template, template_path=self.template_dir))
