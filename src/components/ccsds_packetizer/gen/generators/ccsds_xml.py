import os.path
import models.assembly
from generators import assembly as genassem
from base_classes.generator_base import generator_base
from util import model_loader
from generators.assembly import assembly_hydra_generator, assembly_cosmos_plugin_generator_base


class ccsds_packets_xml(assembly_hydra_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_hydra_generator.__init__(
            self, "name_ccsds_packets.xml", template_dir=template_dir, subdir="Config"
        )

    def generate(self, input_filename):
        a = models.assembly.assembly(input_filename)

        # We need to make sure the CCSDS header and space packet types
        # are included in the assembly. If these types are not in our assembly
        # we need to include them, so mark that.
        if "Ccsds_Primary_Header" in a.complex_types:
            a.ccsds_primary_header_model = None
        else:
            a.ccsds_primary_header_model = model_loader.try_load_model_by_name(
                "Ccsds_Primary_Header"
            )
        if "Ccsds_Space_Packet" in a.complex_types:
            a.ccsds_space_packet_model = None
        else:
            a.ccsds_space_packet_model = model_loader.try_load_model_by_name(
                "Ccsds_Space_Packet"
            )

        a.type_format_dictionary = genassem.type_format_dictionary
        genassem.create_type_print_strings(a)
        genassem.create_type_field_strings(a)
        print(a.render(self.template, template_path=self.template_dir))


class assembly_cosmos_telemetry_txt(assembly_cosmos_plugin_generator_base, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_cosmos_plugin_generator_base.__init__(
            self, "name_ccsds_cosmos_telemetry.txt", template_dir=template_dir, subdir="plugin"
        )

    def generate(self, input_filename):
        a = self.model_cls(input_filename)
        a.ccsds_primary_header_model = model_loader.try_load_model_by_name("Ccsds_Primary_Header")
        a.sys_time_record_model = model_loader.try_load_model_by_name("sys_time")
        genassem.create_type_field_strings(a)
        print(a.render(self.template, template_path=self.template_dir))
