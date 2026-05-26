from models.commands import commands, command
from models.exceptions import ModelException
from models import command_sequences as command_sequences_model
from database.model_database import model_database
from util import ada
from util import model_loader
from util import redo


class sequence_wrapper(object):
    """
    Describes one synthesized per-sequence wrapper command, for consumption by
    the generated intermediate base type (Auto_Instance). Carries everything its
    handler body needs: the runtime sequence index (its slot in the Sequences
    table), the wrapped-argument type it receives, and -- when the sequence takes
    a native argument -- the package used to serialize that argument into the
    Run_Sequence buffer.
    """
    def __init__(self, name, index, has_arg, wrapped_package, native_package):
        self.name = name                      # e.g. Sequence_B
        self.index = index                    # runtime Sequences-table slot
        self.has_arg = has_arg                # whether the sequence takes a native arg
        self.wrapped_package = wrapped_package  # e.g. Sequence_B_Run_Arg
        self.native_package = native_package    # e.g. Sequence_B_Arg (None if no arg)


class simple_sequencer_commands(commands):
    """
    Commands suite for the Simple Command Sequencer component. Loaded from
    files named <component>.simple_sequencer_commands.yaml.

    The static commands declared in the YAML (Run_Sequence, Kill_All_Sequences,
    Set_Summary_Packet_Period) are always present. When the component is built
    in an assembly context, one operator-friendly wrapper command is synthesised
    per command sequence declared anywhere in that build's command_sequences
    suites. Each wrapper carries the sequence's typed wrapped-argument record
    (<Sequence>_Run_Arg.T = the sequence's native args plus a trailing
    Response_Behavior) and, at run time, packs that arg into a Run_Sequence_Arg.T
    and dispatches through the generic Run_Sequence backbone. The wrapper handler
    bodies are supplied by the generated intermediate base type (see
    gen/generators/simple_sequencer_auto_base.py), so they are never hand-written.

    Enrichment happens in load() rather than set_assembly(): the command-suite
    generator renders the model after a plain load() and never calls
    set_assembly(), so discovery must be self-contained. It flows through
    model_database.get_all_models(), which is scoped to the resolved build path.
    A naked `redo build` in the component directory sees no command_sequences
    suites (the test assembly lives under test/, off the default build path) and
    so produces exactly the three static commands. Only a build whose path
    includes an assembly's command_sequences suite adds the per-sequence wrappers.

    Note on build contexts: the generated command suite, component base, and
    implementation all land in this component's shared build/src, so their
    contents reflect whichever context built them last. A *fresh* build in either
    context is always correct, but switching contexts in place (e.g. building the
    test assembly and then building the component standalone without cleaning)
    leaves stale per-sequence artifacts. Run `admt clean` when switching between a
    component-only build and an assembly build. (This is deliberate: forcing
    regeneration on every build via redo-always would recompile the component
    base and everything downstream every time, which is far costlier than an
    occasional clean.)
    """

    def submodel_name(self):
        # Tell the framework to treat this as the component's `commands`
        # suite so all the usual command codegen hooks fire.
        return "commands"

    def load(self):
        # Load the static commands from the YAML.
        super(simple_sequencer_commands, self).load()

        # Descriptors for the generated intermediate base type. Empty unless the
        # build path includes an assembly's command_sequences suite.
        self.sequence_wrappers = []
        # Names used by the Auto_Base templates (set unconditionally so the
        # generated intermediate compiles even in a component-only build).
        # Auto_Base is a CHILD package of the component base so that it can see
        # Base_Instance's private connector-helper primitives (Sys_Time_T_Get,
        # Event_T_Send_If_Connected, etc.), which are only visible to children.
        self.component_package = "Component." + ada.formatType(self.model_name)
        self.auto_base_name = self.component_package + ".Auto_Base"

        # Synthesize one wrapper command per declared sequence in this build
        # context. None are found in a component-only build, so the suite stays
        # at the three static commands.
        synthesized = self._synthesize_sequence_commands()
        if not synthesized:
            return

        # Rebuild the suite from the static commands plus the synthesized
        # wrappers so every derived list (entities, includes, type_models,
        # dependencies) is recomputed and the generated suite + component base
        # both pick up the new commands and their `with` clauses. IDs are still
        # unassigned at load() time (the assembly assigns them later via
        # set_id_base), so re-initializing here is safe.
        static_entities = list(self.entities.values())
        self._initialize(
            name=self.name,
            entity_name="commands",
            entities=static_entities + synthesized,
            description=self.description,
        )
        self.commands = list(self.entities.values())

    def _synthesize_sequence_commands(self):
        """
        Discover every command sequence visible in the current build path and
        return a `command` entity for each. Discovery is redo-native: it queries
        the model database (build-path scoped) rather than scanning the
        filesystem, so it is naturally build-context-aware. Each command_sequences
        suite is loaded standalone -- only sequence names are needed here, not
        assembly resolution.
        """
        with model_database() as db:
            cs_files = sorted(
                m
                for m in db.get_all_models()
                if m.endswith(".command_sequences.yaml")
            )

        synthesized = []
        seen_names = set(self.entities.keys())
        # Runtime sequence index: the slot in the Sequences table that the
        # generated <suite> package builds, in declaration order. (Single
        # command_sequences suite per assembly is assumed -- the runtime wires
        # exactly one Sequences table into the component.)
        seq_index = 0
        for cs_file in cs_files:
            # Declare the dependency so the suite regenerates when sequences change.
            redo.redo_ifchange(cs_file)
            self.dependencies.append(cs_file)

            suite = command_sequences_model.command_sequences(cs_file)
            for seq in suite.sequences.values():
                if seq.name in seen_names:
                    raise ModelException(
                        "Per-sequence command '"
                        + seq.name
                        + "' (from '"
                        + cs_file
                        + "') collides with an existing command of the Simple "
                        "Command Sequencer. Rename the sequence."
                    )
                seen_names.add(seq.name)

                # The wrapper's arg type is the generated wrapped-arg record
                # (<Sequence>_Run_Arg). It is produced by the wrapped_run_args
                # generator into build/yaml, so force it to build before we
                # resolve the type -- otherwise the type model is not yet on
                # disk. Mirrors the fault_correction / task_watchdog pattern.
                type_name = seq.name + "_Run_Arg"
                type_path = model_loader.get_model_file_path(
                    type_name, model_types=["record"]
                )
                if not type_path:
                    raise ModelException(
                        "Could not find the wrapped-argument record model '"
                        + type_name
                        + "' for sequence '"
                        + seq.name
                        + "'. It should be generated by the wrapped_run_args "
                        "generator."
                    )
                redo.redo_ifchange(type_path)

                synthesized.append(
                    command(
                        name=seq.name,
                        type=type_name + ".T",
                        description=(
                            "Run the '"
                            + seq.name
                            + "' command sequence. This operator-friendly wrapper "
                            "packs its typed argument and dispatches through "
                            "Run_Sequence."
                        ),
                        suite=self,
                    )
                )

                # Record the descriptor the Auto_Instance handler body needs.
                self.sequence_wrappers.append(
                    sequence_wrapper(
                        name=seq.name,
                        index=seq_index,
                        has_arg=seq.has_arg(),
                        wrapped_package=type_name,
                        native_package=seq.arg_type_package if seq.has_arg() else None,
                    )
                )
                seq_index += 1
        return synthesized
