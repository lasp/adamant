from base_classes.generator_base import generator_base
from generators.assembly import assembly_generator
from generators.ided_suite import packet_templates
import os.path
from models.simple_command_sequencer_packets import simple_command_sequencer_packets
from models.exceptions import ModelException
from generators.basic import add_basic_generators_to_module
from models import assembly


class assembly_simple_command_sequencer_summary_yaml(assembly_generator, generator_base):
    """Packet yaml type file"""
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self,
            "name_simple_command_sequencer_summary_packet_type.record.yaml",
            template_dir=template_dir,
        )

    def generate(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        assem.num_simple_command_sequencer_frames = 0
        # Go through the assembly and collect the frame count of every
        # sequencer instance. The summary packet type is generated once per
        # assembly, so every instance must agree on Num_Concurrent_Sequences --
        # otherwise the shared type would mis-describe one instance's packet on
        # the ground. Reject mixed configurations at build time.
        frame_counts = {}
        for component in assem.components.values():
            if component.name == "Simple_Command_Sequencer":
                value = component.init.get_parameter_value("Num_Concurrent_Sequences")
                try:
                    num_frames = int(value)
                except (TypeError, ValueError):
                    raise ModelException(
                        "Simple_Command_Sequencer instance '"
                        + component.instance_name
                        + "' must be initialized with a literal integer "
                        + "Num_Concurrent_Sequences to generate its summary "
                        + "packet type, found: '" + str(value) + "'."
                    )
                frame_counts[component.instance_name] = num_frames
        if frame_counts:
            if len(set(frame_counts.values())) > 1:
                raise ModelException(
                    "All Simple_Command_Sequencer instances in an assembly must "
                    "be initialized with the same Num_Concurrent_Sequences, "
                    "since they share one generated summary packet type. Found: "
                    + ", ".join(
                        f"{name}={count}" for name, count in sorted(frame_counts.items())
                    )
                )
            # Save num_frames into the assembly so it can be referenced in template:
            assem.num_simple_command_sequencer_frames = next(iter(frame_counts.values()))
        print(assem.render(self.template, self.template_dir))

    def depends_on(self, input_filename):
        assem = assembly.assembly(filename=input_filename, shallow_load=True)
        return assem.get_dependencies()


# Add all the basic generators for normal packets, except use the
# simple_command_sequencer_packets model. This allows us to build all the
# normal packet outputs like html, tex, etc.
add_basic_generators_to_module(
    simple_command_sequencer_packets,
    packet_templates,
    module=globals(),
)
