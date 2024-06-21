from base_classes.generator_base import generator_base
from generators.assembly import assembly_generator
from generators.ided_suite import packet_templates
import os.path
from models import command_sequencer_packets
from generators.basic import add_basic_generators_to_module
from models import assembly


class assembly_command_sequencer_summary_yaml(assembly_generator, generator_base):
    """Packet yaml type file"""
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self,
            "name_command_sequencer_summary_packet_type.record.yaml",
            template_dir=template_dir,
        )

    def generate(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        assem.num_command_sequencer_engines = 0
        # Go through assembly to find sequencer component. When we find it extract its init
        # parameters which are needed in the template:
        for component in assem.components.values():
            if component.name == "Command_Sequencer":
                num_engines = int(component.init.get_parameter_value("Num_Engines"))
                # Save num_engines into the assembly so it can be referenced in template:
                if num_engines:
                    assem.num_command_sequencer_engines = num_engines
        print(assem.render(self.template, self.template_dir))

    def depends_on(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        return assem.get_dependencies()


class assembly_command_sequencer_details_yaml(assembly_generator, generator_base):
    """Packet yaml type file"""
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self,
            "name_command_sequencer_details_packet_type.record.yaml",
            template_dir=template_dir,
        )

    def generate(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        assem.num_command_sequencer_engine_stack_size = 0
        # Go through assembly to find sequencer component. When we find it extract its init
        # parameters which are needed in the template:
        for component in assem.components.values():
            if component.name == "Command_Sequencer":
                stack_size = int(component.init.get_parameter_value("Stack_Size"))
                # Save num_engines into the assembly so it can be referenced in template:
                if stack_size:
                    assem.num_command_sequencer_engine_stack_size = stack_size
        print(assem.render(self.template, self.template_dir))

    def depends_on(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        return assem.get_dependencies()


# Add all the basic generators for normal packets, except use the command_sequencer_packets model.
# This allows us to build all the normal packet outputs like html, tex, etc.
add_basic_generators_to_module(
    command_sequencer_packets.command_sequencer_packets,
    packet_templates,
    module=globals(),
)
add_basic_generators_to_module(
    assembly.assembly,
    ["assembly/name_seq.html"],
    module=globals(),
    template_dir=os.path.join(
        os.path.dirname(os.path.realpath(__file__)), ".." + os.sep + "templates"
    ),
)


class assembly_seq_cmd_tlm_txt(assembly_generator, generator_base):
    """Generator for SEQ command and telemetry configuration file."""
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self, "name_seq_cmd_tlm.txt", template_dir=template_dir
        )

    def generate(self, input_filename, methods_to_call_on_model_obj=[]):
        assembly_generator.generate(
            self,
            input_filename,
            methods_to_call_on_model_obj=[
                "load_command_type_ranges",
                "load_data_product_type_ranges",
            ]
            + methods_to_call_on_model_obj,
        )
