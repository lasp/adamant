import os.path
from models.simple_sequencer_commands import simple_sequencer_commands
from base_classes.generator_base import generator_base
from generators.basic import basic_generator


class _impl_gen(basic_generator):
    """
    Generates the Simple Command Sequencer's implementation spec/body from the
    simple_sequencer_commands model. The body forwards every static primitive to
    the hand-written Implementation.Logic child and emits an inline handler for
    each synthesized per-sequence wrapper command. In a component-only build there
    are no wrappers, so this is exactly the three static commands.

    Output is forced into build/src (basic_generator would otherwise route
    "-implementation" files to build/template, which is not compiled).
    """
    def __init__(self, template_filename):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = [os.path.join(this_file_dir, ".." + os.sep + "templates")]
        basic_generator.__init__(
            self,
            model_class=simple_sequencer_commands,
            template_filename=template_filename,
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )
        a = self.template_basename.rsplit("name", maxsplit=1)
        out = model_name.lower().join(a)
        return dirname + os.sep + "build" + os.sep + "src" + os.sep + out


class simple_command_sequencer_implementation_ads(_impl_gen, generator_base):
    def __init__(self):
        _impl_gen.__init__(self, template_filename="component-name-implementation.ads")


class simple_command_sequencer_implementation_adb(_impl_gen, generator_base):
    def __init__(self):
        _impl_gen.__init__(self, template_filename="component-name-implementation.adb")
