from base_classes.generator_base import generator_base
from generators import assembly as assembly_gen
import os.path
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
        assembly_gen.create_type_field_strings(a)
        print(a.render(self.template, template_path=self.template_dir))
