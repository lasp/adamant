from models.commands import commands
from models.exceptions import ModelException, throw_exception_with_filename
from util import model_loader
import os


class simple_command_sequencer_commands(commands):
    def submodel_name(self):
        # Tell the framework to treat this as the component's `commands`
        # suite so all the usual command codegen hooks fire.
        return "commands"

    def load(self):
        super(simple_command_sequencer_commands, self).load()
        # Per-sequence commands are injected per assembly in set_assembly, so never
        # serve this suite from the session-shared model cache (see assembly.py:550).
        self.do_save_to_cache = False

    # Errors raised here happen outside the base class's load path, so attach
    # the yaml filename to them explicitly for context.
    @throw_exception_with_filename
    def set_assembly(
        self, assembly
    ):  # Make sure an assembly is set by the base class implementation.
        # Set assembly:
        self.assembly = assembly

        # Find the command_sequences model this instance was initialized with.
        # First get the package name from the instance's Config init
        # parameter:
        configs = self.component.init.get_parameter_value("Config")
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
                "Could not find a model file for command_sequences model: "
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

        # Injected commands keep .suite on the command_sequences model (it backs
        # command_timeout_millis and the templates). The assembly dictionary template
        # renders command.suite.component.instance_name, so give that model a
        # .component back-pointer.
        self.command_sequences_model.component = self.component

        # Provide the assembly to the product packetizer model
        self.command_sequences_model.set_assembly(assembly)

        # Each sequence becomes a command on the sequencer instance, so a sequence
        # may not share a name with one of the built-in commands already in the suite.
        built_in_names = list(self.entities.keys())
        for seq in self.command_sequences_model.sequences.values():
            if seq.name in self.entities:
                raise ModelException(
                    f'Sequence "{seq.name}" is named like a built-in command of the '
                    f'Simple Command Sequencer. Each sequence becomes a command on '
                    f'the sequencer instance, so a sequence may not be named any of: '
                    + ", ".join(built_in_names) + ".",
                    filename=self.command_sequences_model.full_filename,
                    lineno=seq.lineno,
                )

        # Inject the per-sequence commands. Ids are stamped later by the assembly-wide
        # id pass, together with the built-in commands.
        self.entities.update(self.command_sequences_model.sequences)
        self.ids = [e.id for e in self.entities.values() if e.id]

        # The injected commands carry user-written arg types the component never
        # references, so they are missing from the component's complex_types (built
        # before injection) and assembly consumers such as the Hydra command config
        # would KeyError. Register each resolved type_model (and embedded types) now.
        # Plain dict update of resolved models: no model loads, no redo-ifchange.
        for cmd in self.command_sequences_model.sequences.values():
            type_model = cmd.type_model
            if type_model is not None:
                self.component.complex_types[type_model.name] = type_model
                for embedded in type_model.get_all_type_models_recursive():
                    self.component.complex_types[embedded.name] = embedded

        # Call the base class version:
        super(simple_command_sequencer_commands, self).set_component(self.component)
        super(simple_command_sequencer_commands, self).set_assembly(assembly)

    @throw_exception_with_filename
    def final(self):
        # Call final on the command sequences model to resolve its steps
        # against the assembly.
        self.command_sequences_model.final()
