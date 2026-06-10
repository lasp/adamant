import os.path
from models.command_sequences import command_sequences
from base_classes.generator_base import generator_base
from generators.basic import basic_generator, add_basic_generators_to_module
from util import redo_arg


def load_command_sequences_model(input_filename):
    """
    Load a command sequences model fully resolved against its assembly.
    """
    cs = command_sequences(input_filename)
    dirname, view_name, assembly_name, *ignore = redo_arg.split_model_filename(
        cs.full_filename
    )
    from util import model_loader
    assembly = model_loader.try_load_model_by_name(
        assembly_name, model_types="assembly"
    )
    if assembly:
        cs.set_assembly(assembly)
        # Step resolution, includes, and assembly_name are populated in final()
        # (set_assembly only attaches the assembly). The assembly load path gets
        # final() called for it automatically, but this standalone generator
        # must drive it itself so name.ads/adb render fully resolved.
        cs.final()
    return cs


class command_sequences_gen(basic_generator):
    """Base generator class for command sequences."""

    def __init__(self, template_filename):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = [os.path.join(this_file_dir, ".." + os.sep + "templates")]
        basic_generator.__init__(
            self,
            model_class=command_sequences,
            template_filename=template_filename,
            template_dir=template_dir,
        )

    def generate(self, input_filename):
        cs = load_command_sequences_model(input_filename)
        print(cs.render(self.template, template_path=self.template_dir))

    def output_filename(self, input_filename):
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )
        build_dir = self._get_default_build_dir()
        cs = command_sequences(input_filename)
        base_name = cs.name.lower()
        # Substitute "name" in the template basename with the actual model name
        a = self.template_basename.rsplit("name", maxsplit=1)
        output_fname = base_name.join(a)
        return dirname + os.sep + build_dir + os.sep + output_fname


class command_sequences_ads(command_sequences_gen, generator_base):
    """
    Generates <model_name>.ads – a package spec that declares a fully
    initialised Sequences_Type constant (and a stable Sequences_Access
    pointer) using Simple_Sequencer_Types.
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name.ads")


class command_sequences_adb(command_sequences_gen, generator_base):
    """
    Generates <model_name>.adb – the body that implements the per-dynamic-step
    Resolver functions. It is only emitted when the suite has at least one
    dynamic step; with no dynamic steps the spec declares no subprograms, so a
    body package would be empty and illegal (a body is not allowed for a spec
    that does not require one).
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name.adb")

    def output_filename(self, input_filename):
        # Suppress the body unless there is a dynamic step. Dynamic steps are
        # detected at load time (from the YAML "Arg.*" pattern), so no assembly
        # resolution is needed here.
        cs = command_sequences(input_filename)
        if not cs.has_dynamic_steps():
            return ""
        return command_sequences_gen.output_filename(self, input_filename)


class command_sequences_command_builders_ads(command_sequences_gen, generator_base):
    """
    Generates <model_name>_command_builders.ads -- an instantiable helper that
    reconstructs the operator-side builder surface (per-sequence command id
    getters + Command.T constructors) for the per-sequence "ghost" commands,
    which have no generated handler on the component itself. Used by unit tests
    and sub-sequence callers.
    """

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name_command_builders.ads")


class command_sequences_command_builders_adb(command_sequences_gen, generator_base):
    """Body for the per-sequence command builder helper."""

    def __init__(self):
        command_sequences_gen.__init__(self, template_filename="name_command_builders.adb")


def add_generators_to_module(module):
    add_basic_generators_to_module(module, [
        command_sequences_ads,
        command_sequences_adb,
        command_sequences_command_builders_ads,
        command_sequences_command_builders_adb,
    ])
