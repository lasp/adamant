from models.commands import commands


class simple_sequencer_commands(commands):
    """
    Commands suite for the Simple Command Sequencer component. Loaded from
    files named <component>.simple_sequencer_commands.yaml.

    Inherits the standard commands schema/codegen path. The static commands
    declared in the YAML are always present. Per-sequence wrapper commands
    are layered in only when this component is built in an assembly context
    (the assembly supplies the command_sequences suite); that enrichment is
    still being designed -- see the Phase 1.5 discussion.
    """

    def submodel_name(self):
        # Tell the framework to treat this as the component's `commands`
        # suite so all the usual command codegen hooks fire.
        return "commands"
