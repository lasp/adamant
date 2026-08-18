import os.path
from models.command_sequences import command_sequences
from models.simple_command_sequencer_commands import simple_command_sequencer_commands
from models.simple_command_sequencer_packets import (
    simple_command_sequencer_packets,
    get_max_frames_per_packet,
)
from base_classes.generator_base import generator_base
from generators.basic import basic_generator, add_basic_generators_to_module
from generators.ided_suite import command_templates, packet_templates
from util import redo_arg
from util import ada
from util import model_loader


def load_command_sequences_model(input_filename):
    """
    Load a command sequences model fully resolved against its assembly.
    """
    cs = command_sequences(input_filename)
    dirname, view_name, assembly_name, *ignore = redo_arg.split_model_filename(
        cs.full_filename
    )
    assembly_model = model_loader.try_load_model_by_name(
        assembly_name, model_types="assembly"
    )
    if assembly_model:
        cs.set_assembly(assembly_model)
        # Step resolution, includes, and assembly_name are populated in final()
        # (set_assembly only attaches the assembly). The assembly load path gets
        # final() called for it automatically, but this standalone generator
        # must drive it itself so name.ads/adb render fully resolved.
        cs.final()
    return cs


class command_sequences_gen(basic_generator):
    """Base generator class for command sequences outputs."""

    def __init__(self, template_filename):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = [os.path.join(this_file_dir, ".." + os.sep + "templates")]
        basic_generator.__init__(
            self,
            model_class=command_sequences,
            template_filename=template_filename,
            template_dir=template_dir,
        )

    def _suite_package_name(self, input_filename):
        """
        Compute the generated suite package name directly from the input
        filename parts. This formula mirrors command_sequences.load()'s
        self.name, without constructing the model.
        """
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )
        base_name = ada.formatType(model_name) + "_Command_Sequences"
        if specific_name:
            base_name = base_name + "_" + ada.formatVariable(specific_name)
        return base_name

    def generate(self, input_filename):
        cs = load_command_sequences_model(input_filename)
        print(cs.render(self.template, template_path=self.template_dir))

    def output_filename(self, input_filename):
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )
        build_dir = self._get_default_build_dir()
        # Compute the output name from the filename parts rather than
        # constructing the command_sequences model, so redo's DB-setup pass
        # (which calls output_filename for every rule) never triggers model
        # loads.
        base_name = self._suite_package_name(input_filename).lower()
        # Substitute "name" in the template basename with the actual model name
        a = self.template_basename.rsplit("name", maxsplit=1)
        output_fname = base_name.join(a)
        return dirname + os.sep + build_dir + os.sep + output_fname


class command_sequences_ads(command_sequences_gen, generator_base):
    """
    Generates <suite_package>.ads – a package spec that declares a fully
    initialised Sequences_Type constant (and a stable Sequences_Access
    pointer), the To_Arg buffer-padding helper, and the per-dynamic-step
    Resolver function declarations.
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name.ads")


class command_sequences_adb(command_sequences_gen, generator_base):
    """
    Generates <suite_package>.adb – the body implementing To_Arg and the
    per-dynamic-step Resolver functions. When the suite needs neither, the
    body degenerates to "pragma No_Body;".
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name.adb")


class command_sequences_commands_ads(command_sequences_gen, generator_base):
    """
    Generates <suite_package>_commands.ads – the operator-side command builder
    surface (id getters and Command.T constructors) for the suite's
    per-sequence ghost commands, used by unit tests and other on-board
    callers. Split from the sequences package because the sequencer component
    itself never uses the builders.
    """

    def __init__(self):
        command_sequences_gen.__init__(
            self, template_filename="name_commands.ads"
        )


class command_sequences_commands_adb(command_sequences_gen, generator_base):
    """
    Generates <suite_package>_commands.adb – the command builder bodies.
    """

    def __init__(self):
        command_sequences_gen.__init__(
            self, template_filename="name_commands.adb"
        )


# The summary packet's ground/documentation type is one Sequence_Frame_Summary
# field per frame. Its layout depends only on the frame count -- not on the
# sequence suite or the instance -- so a single family of per-count record
# types (Simple_Sequencer_Summary_Record_<N>) serves every sequencer instance.
# Each instance's packets model resolves its packet type from its own
# Num_Concurrent_Sequences init parameter (see
# simple_command_sequencer_packets.py), so instances sharing a sequence suite
# are free to size their frame pools independently.
#
# Rule registration is gated by the REAL frame-count bound -- the number of
# frame summaries that fit in the project's configured packet buffer
# (get_max_frames_per_packet, the Python mirror of
# Num_Concurrent_Sequences_Type'Last). The model database is built before
# generator rules are enumerated precisely so generators can query it like
# this, and the bound tracks the project configuration automatically: growing
# the packet buffer in the configuration YAML registers more record rules
# with no framework change. Only counts actually referenced by an assembly
# are ever demanded and built.
#
# The Python class family itself must be sized before any database exists, so
# its ceiling is set by the wire format rather than configuration: a CCSDS
# packet length field is 16 bits (buffer <= 65536 bytes) and a
# Sequence_Frame_Summary can never be smaller than 8 bytes, so no
# configuration can ever need more than 8192 record types.
_SUMMARY_RECORD_FAMILY_CEILING = 8192

# Cache for the computed registration bound (per python invocation).
_summary_record_bound = [None]


def _get_summary_record_bound():
    if _summary_record_bound[0] is None:
        try:
            _summary_record_bound[0] = get_max_frames_per_packet()
        except Exception:
            # If the model database is unavailable in this context, fall back
            # to registering the whole family -- unused rules are never built.
            _summary_record_bound[0] = _SUMMARY_RECORD_FAMILY_CEILING
    return _summary_record_bound[0]


def _make_summary_record_generator(num_frames):
    class summary_record_generator(generator_base):
        frames = num_frames

        def input_file_regex(self):
            # Match only the component's own packet suite file so the record
            # family generates exactly once, in this component's build
            # directory, shared by every assembly in the build path.
            return (
                r".*/simple_command_sequencer\."
                r"simple_command_sequencer_packets\.yaml$"
            )

        def output_filename(self, input_filename):
            # Register a rule only for counts within the real, buffer-derived
            # bound. Returning an empty name suppresses the rule.
            if self.frames > _get_summary_record_bound():
                return ""
            dirname = os.path.dirname(input_filename)
            return (
                dirname + os.sep + "build" + os.sep + "yaml" + os.sep
                + "simple_sequencer_summary_record_" + str(self.frames)
                + ".record.yaml"
            )

        def generate(self, input_filename):
            lines = [
                "---",
                "description: This is an autocoded summary packet type for a "
                "Simple Command Sequencer instance initialized with "
                "Num_Concurrent_Sequences => " + str(self.frames) + ". It "
                "contains one Sequence_Frame_Summary record per sequence "
                "frame, in frame order.",
                "fields:",
            ]
            for frame in range(self.frames):
                lines.append("  - name: Frame_" + str(frame) + "_Summary")
                lines.append(
                    '    description: "A summary of the state of sequence '
                    'frame ' + str(frame) + '."'
                )
                lines.append("    type: Sequence_Frame_Summary.T")
            print("\n".join(lines))

    summary_record_generator.__name__ = (
        "simple_sequencer_summary_record_" + str(num_frames) + "_record_yaml"
    )
    return summary_record_generator


for _num_frames in range(1, _SUMMARY_RECORD_FAMILY_CEILING + 1):
    _generator_class = _make_summary_record_generator(_num_frames)
    globals()[_generator_class.__name__] = _generator_class


# Register the standard commands code generators (.ads, .adb, .html) for the
# component's *.simple_command_sequencer_commands.yaml suite. The framework
# derives the input_file_regex from the model class name, so this is what
# makes the file's build rules appear.
add_basic_generators_to_module(
    simple_command_sequencer_commands, command_templates, module=globals()
)

# Register the standard packet outputs (html, tex, etc.) for the component's
# *.simple_command_sequencer_packets.yaml suite.
add_basic_generators_to_module(
    simple_command_sequencer_packets, packet_templates, module=globals()
)
