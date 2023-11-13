from base_classes.generator_base import generator_base
from generators.assembly import assembly_generator, assembly_hydra_generator
from generators.ided_suite import packet_templates
import os.path
from models import queue_monitor_packets
from models import assembly
from generators.basic import add_basic_generators_to_module


# Packet yaml type file
class assembly_queue_monitor_yaml(assembly_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self,
            "name_queue_monitor_packet_type.record.yaml",
            template_dir=template_dir,
        )

    def generate(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        print(assem.render(self.template, self.template_dir))

    def depends_on(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        return assem.get_dependencies()


# Hydra file
class assembly_queue_monitor_xml(assembly_hydra_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_hydra_generator.__init__(
            self, "name_queue_monitor.xml", template_dir=template_dir, subdir="Pages"
        )


# Add all the basic generators for normal packets, except use the queue_monitor_packets model.
# This allows us to build all the normal packet outputs like html, tex, etc.
add_basic_generators_to_module(
    queue_monitor_packets.queue_monitor_packets, packet_templates, module=globals()
)
