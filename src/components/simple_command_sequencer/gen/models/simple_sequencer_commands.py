from models.commands import commands


class simple_sequencer_commands(commands):
    """
    Commands suite for the Simple Command Sequencer component. Loaded from
    files named <component>.simple_sequencer_commands.yaml.

    Inherits the standard commands schema/codegen path. Phase 1 of the
    Simple Command Sequencer refactor will extend set_assembly here to
    synthesise one command per declared sequence in the owning assembly's
    command_sequences suite.
    """

    def submodel_name(self):
        # Tell the framework to treat this as the component's `commands`
        # suite so all the usual command codegen hooks fire.
        return "commands"
