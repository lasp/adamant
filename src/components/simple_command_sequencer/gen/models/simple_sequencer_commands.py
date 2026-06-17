from models.commands import commands
from models.exceptions import ModelException
from util import model_loader
import os


class simple_sequencer_commands(commands):
    def submodel_name(self):
        # Tell the framework to treat this as the component's `commands`
        # suite so all the usual command codegen hooks fire.
        return "commands"

    def load(self):
        super(simple_sequencer_commands, self).load()
        # The per-sequence commands are injected per-assembly in set_assembly, so
        # this suite is context-dependent. Never serve it from the (filename+mtime-
        # keyed, session-shared) model cache, or a standalone 3-command load gets
        # reused for an assembly's command dictionary. (Framework precedent: assembly.py:550.)
        self.do_save_to_cache = False

    def set_assembly(
        self, assembly
    ):  # Make sure an assembly is set by the base class implementation.
        # Set assembly:
        self.assembly = assembly

        # Get the model for the product packetizer so that we can create the packet list. First
        # get the package name:
        configs = self.component.init.get_parameter_value("Sequences")
        command_sequences_package = configs.split(".")[0]
        # Based on the package name figure out the model name:
        split_package = command_sequences_package.split("_Command_Sequences")
        command_sequences_model_name = split_package[0]
        specific_name = None
        if len(split_package) > 1:
            specific_name = split_package[1][1:]

        # Get the model file paths:
        model_paths = model_loader.get_model_file_paths(
            command_sequences_model_name, model_types="command_sequences"
        )
        if not model_paths:
            raise ModelException(
                "Could not model for command_sequences model: "
                + command_sequences_model_name
            )

        # Figure out which path this init argument is referring to:
        model_path = None
        if specific_name:
            for p in model_paths:
                sp = os.path.basename(p).split(".")
                if len(sp) > 3 and sp[0].lower() == specific_name.lower():
                    model_path = p
                    break
        else:
            for p in model_paths:
                sp = os.path.basename(p).split(".")
                if len(sp) == 3:
                    model_path = p
                    break

        # Load the model from the path:
        self.command_sequences_model = model_loader.load_model(model_path)

        # The per-sequence command entities keep their .suite pointing at the
        # command_sequences model (it backs command_timeout_millis and the
        # name.ads/adb templates). The assembly command-dictionary template
        # renders `command.suite.component.instance_name`, so the cs model needs
        # a .component back-pointer to this instance's component for that to
        # resolve. (Re-parenting .suite to this commands suite would break the
        # cs model's own timeout/template lookups.)
        self.command_sequences_model.component = self.component

        # Provide the assembly to the product packetizer model
        self.command_sequences_model.set_assembly(assembly)

        # Set the component packets to the product packet model packets.
        self.entities.update(self.command_sequences_model.sequences)
        self.ids = [e.id for e in self.entities.values() if e.id]

        # Call the base class version:
        super(simple_sequencer_commands, self).set_component(self.component)
        super(simple_sequencer_commands, self).set_assembly(assembly)

    def final(self):
        # Call final on the product packetizer to finalize the packets.
        self.command_sequences_model.final()
