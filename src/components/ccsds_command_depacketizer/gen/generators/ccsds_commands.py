from base_classes.generator_base import generator_base
from generators import assembly as assembly_gen
import os.path
from util import model_loader
from generators.assembly import assembly_hydra_generator, assembly_cosmos_plugin_generator_base

# This module contains a generators which produces
# the hydra xml for producing commands that adhere to the
# ccsds_command_depacketizer expected format.


class assembly_ccsds_commands_xml(assembly_hydra_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_hydra_generator.__init__(
            self, "name_ccsds_commands.xml", template_dir=template_dir, subdir="Config"
        )

    def generate(self, input_filename):
        a = self.model_cls(input_filename)
        if "Ccsds_Command_Depacketizer" in a.component_types_dict:
            a.additional_types.append("Ccsds_Command_Secondary_Header.T")
        assembly_gen.create_type_field_strings(a)
        print(a.render(self.template, template_path=self.template_dir))


class assembly_cosmos_commands_txt(assembly_cosmos_plugin_generator_base, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_cosmos_plugin_generator_base.__init__(
            self, "name_ccsds_cosmos_commands.txt", template_dir=template_dir, subdir="plugin"
        )

    def generate(self, input_filename):
        a = self.model_cls(input_filename)
        a.ccsds_primary_header_model = model_loader.try_load_model_by_name("Ccsds_Primary_Header")
        a.ccsds_command_secondary_header = model_loader.try_load_model_by_name("Ccsds_Command_Secondary_Header")
        assembly_gen.create_type_field_strings(a)
        print(a.render(self.template, template_path=self.template_dir))
