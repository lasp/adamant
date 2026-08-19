import os.path
from models.command_sequences import command_sequences
from models.simple_command_sequencer_commands import simple_command_sequencer_commands
from models.simple_command_sequencer_packets import simple_command_sequencer_packets
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


class command_sequences_summary_record(command_sequences_gen, generator_base):
    """
    Generates <suite_package>_summary_record.record.yaml -- the summary
    packet's ground/documentation type for this suite: one
    Sequence_Frame_Summary field per sequence frame (the suite's
    num_concurrent_sequences, in frame order). Generated per suite, right
    next to the suite's other outputs, so the frame pool and the packet
    layout come from the same model value and can never disagree. The FSW
    never withs this type; the packets model resolves an instance's
    Summary_Packet type to it via the instance's Config init parameter.
    """

    def __init__(self):
        command_sequences_gen.__init__(
            self, template_filename="name_summary_record.record.yaml"
        )

    def generate(self, input_filename):
        # Load the suite model WITHOUT resolving it against its assembly: the
        # record's layout depends only on the suite's own
        # num_concurrent_sequences, and the assembly load path itself demands
        # this record (via the packets model), so resolving against the
        # assembly here would create a circular dependency.
        cs = command_sequences(input_filename)
        print(cs.render(self.template, template_path=self.template_dir))


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
