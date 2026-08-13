import os.path
from models.command_sequences import command_sequences
from models.simple_command_sequencer_commands import simple_command_sequencer_commands
from models.simple_command_sequencer_packets import simple_command_sequencer_packets
from models.exceptions import ModelException
from models import assembly
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
    pointer) plus the per-sequence command builder surface (id getters and
    Command.T constructors) used by unit tests and other on-board callers.
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name.ads")


class command_sequences_adb(command_sequences_gen, generator_base):
    """
    Generates <suite_package>.adb – the body implementing the command builders
    (always present) and the per-dynamic-step Resolver functions (when the
    suite has dynamic steps). The builders guarantee the spec always requires
    a body, so this pair is emitted unconditionally.
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name.adb")


class command_sequences_record_yaml(command_sequences_gen, generator_base):
    """
    Generates <suite_package>_record.record.yaml – the packed record type for
    the summary packet of any sequencer instance initialized with this suite
    (one Sequence_Frame_Summary field per frame). The record is sized by the
    Num_Concurrent_Sequences init parameter of the referencing instance(s) in
    the assembly named by this suite's filename, so each suite -- and
    therefore each instance -- gets its own correctly-sized ground type. The
    packets model resolves the type as <Sequences package>_record from the
    instance's init parameter (see simple_command_sequencer_packets.py).
    """

    def __init__(self):
        command_sequences_gen.__init__(
            self, template_filename="name_record.record.yaml"
        )

    def _shallow_assembly(self, input_filename):
        """
        Shallow-load the assembly named in this suite's filename
        (<specific>.<assembly>.command_sequences.yaml). A shallow load parses
        component instances and their init parameters without loading
        component submodels -- a full load would recurse, since loading the
        assembly loads the sequencer's packets model, which redo_ifchange's
        this very record. Returns None if no such assembly model exists.
        """
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )
        assembly_path = model_loader.get_model_file_path(
            model_name, model_types=["assembly"]
        )
        if not assembly_path:
            return None
        return assembly.assembly(filename=assembly_path, shallow_load=True)

    def generate(self, input_filename):
        suite_package = self._suite_package_name(input_filename)
        assem = self._shallow_assembly(input_filename)
        if assem is None:
            raise ModelException(
                "Could not find an assembly model named after command sequences "
                "file '" + input_filename + "' to size its summary packet record."
            )

        # Collect the frame count of every sequencer instance initialized with
        # THIS suite. Instances that share a suite share its generated record
        # type, so they must agree on Num_Concurrent_Sequences -- give each a
        # suite of its own to size them differently.
        frame_counts = {}
        for component in assem.components.values():
            if component.name == "Simple_Command_Sequencer":
                sequences_value = component.init.get_parameter_value("Sequences")
                if (
                    not sequences_value
                    or sequences_value.split(".")[0].lower() != suite_package.lower()
                ):
                    continue
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

        num_frames = 0
        if frame_counts:
            if len(set(frame_counts.values())) > 1:
                raise ModelException(
                    "Simple_Command_Sequencer instances initialized with the "
                    "same sequence suite '" + suite_package + "' must agree on "
                    "Num_Concurrent_Sequences, since they share the suite's "
                    "generated summary packet type. Found: "
                    + ", ".join(
                        f"{name}={count}"
                        for name, count in sorted(frame_counts.items())
                    )
                    + ". Give each instance its own command sequences suite to "
                    "size them differently."
                )
            num_frames = next(iter(frame_counts.values()))

        # Render the record template with the frame count and suite name. With
        # no referencing instance the template emits a placeholder field --
        # the record is never demanded by the build in that case.
        assem.num_simple_command_sequencer_frames = num_frames
        assem.command_sequences_suite_name = suite_package
        print(assem.render(self.template, self.template_dir))

    def depends_on(self, input_filename):
        # The record's content is a function of the assembly's init parameters
        # (which instances reference this suite and their frame counts), so
        # depend on the assembly model rather than the suite model.
        assem = self._shallow_assembly(input_filename)
        if assem is not None:
            return assem.get_dependencies()
        return []


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
