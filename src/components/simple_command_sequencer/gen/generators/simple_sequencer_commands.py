from generators.basic import add_basic_generators_to_module
from generators.ided_suite import command_templates
from models.simple_sequencer_commands import simple_sequencer_commands

# Register the standard commands code generators (.ads, .adb, .html) for YAML
# files matching `*.simple_sequencer_commands.yaml`. The framework derives the
# input_file_regex from the model class name, so this is what makes the new
# file's build rules appear.
add_basic_generators_to_module(
    simple_sequencer_commands, command_templates, module=globals()
)
