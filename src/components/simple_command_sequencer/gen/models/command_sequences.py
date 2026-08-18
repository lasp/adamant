from util import ada
import os.path
from collections import OrderedDict
from models.exceptions import (
    ModelException,
    throw_exception_with_lineno,
    throw_exception_with_filename,
)
from models.assembly import assembly_submodel
from models.commands import (
    command
)
from util import model_loader
import re

DEFAULT_COMMAND_TIMEOUT_SECONDS = 30


class sequence_step(object):
    """
    Represents a single step in a command sequence.
    """

    # Regex that identifies a dynamic arg value — "Arg" followed by one or
    # more ".Identifier" segments.  Anything else is treated as a static Ada
    # expression.
    _DYNAMIC_ARG_RE = re.compile(r'^Arg(\.[A-Za-z][A-Za-z0-9_]*)*$')

    # Static sleeps are bounded to Ada's Natural so the duration always fits
    # an Ada.Real_Time.Time_Span by construction — no runtime range check.
    MAX_STATIC_SLEEP_MS = 2**31 - 1

    def __init__(
        self,
        command=None,
        arg=None,
        wait_for_completion=None,
        sleep_ms=None,
    ):
        # A step is either a command dispatch or a sleep, never both. The
        # parser/validator enforces this so downstream code can branch on
        # is_sleep() / is_command().
        self.command = command
        # sleep_ms is either a static integer millisecond count or a dynamic
        # reference into the sequence's argument ("Arg" or "Arg.A.B"), resolved
        # at execution time as a Packed_U32.
        self.sleep_ms = None
        self.dynamic_sleep_arg = None
        if sleep_ms is not None:
            if isinstance(sleep_ms, int) and not isinstance(sleep_ms, bool):
                self.sleep_ms = sleep_ms
            elif self._DYNAMIC_ARG_RE.match(str(sleep_ms).strip()):
                self.dynamic_sleep_arg = str(sleep_ms).strip()
            else:
                raise ModelException(
                    f"sleep_ms value '{sleep_ms}' must be an integer or a "
                    "dynamic argument reference of the form 'Arg' or "
                    "'Arg.Field_Name'"
                )
        if command is not None:
            self.parse_command()
        else:
            self.component_name = None
            self.command_name = None
        # If arg matches the dynamic pattern (e.g. "Arg.A.B.C") it is stored
        # in dynamic_arg and arg is cleared; otherwise it stays in arg.
        if arg is not None and self._DYNAMIC_ARG_RE.match(arg):
            self.arg = None
            self.dynamic_arg = arg
        else:
            self.arg = arg
            self.dynamic_arg = None
        self._wait_for_completion = wait_for_completion

        # Set during assembly resolution:
        self.component = None
        self.command_obj = None
        self.index = 0
        # The Ada package that owns the arg type's Serialization child, e.g.
        # "Command_Router_Arg" for a step whose arg_type is "Command_Router_Arg.T"
        self.arg_type_package = None

        # Dynamic step resolution fields, populated by resolve_dynamic_arg_type:
        #   input_type_package  - Ada package of the sequence-level arg type
        #                         e.g. "My_Input_Type"
        #   traversal_path      - dotted Ada field path after the root
        #                         e.g. "A.B.C.D" for "Arg.A.B.C.D"
        #   dynamic_arg_type_package - Ada package of the leaf field type
        #                         e.g. "Sys_Time_32"
        self.input_type_package = None
        self.traversal_path = None
        self.dynamic_arg_type_package = None
        self.resolver_type_name = None
        self.resolver_instance_name = None

    def parse_command(self):
        if not re.match(
            r'^[A-Za-z][A-Za-z0-9_]*\.[A-Za-z][A-Za-z0-9_]*$',
            self.command
        ):
            raise ModelException(
                f"Command '{self.command}' must be in format 'Component.Command_Name' "
                "where both parts are valid Ada identifiers"
            )
        parts = self.command.split(".")
        self.component_name = parts[0]
        self.command_name = parts[1]

    def set_defaults(self, parent_sequence):
        if self._wait_for_completion is None:
            self.wait_for_completion = parent_sequence.wait_for_command_completion
        else:
            self.wait_for_completion = self._wait_for_completion

    def validate(self):
        # Mutual exclusivity: exactly one of command/sleep_ms.
        if self.command is None and not self.is_sleep():
            raise ModelException(
                f"Step {self.index} must specify either 'command' or 'sleep_ms'"
            )
        if self.command is not None and self.is_sleep():
            raise ModelException(
                f"Step {self.index} cannot specify both 'command' and 'sleep_ms'"
            )
        # Sleep-form: no arg/wait_for_completion fields allowed, and static
        # durations must fit a Natural (and thus an Ada.Real_Time.Time_Span).
        if self.is_sleep():
            if self.arg is not None or self.dynamic_arg is not None:
                raise ModelException(
                    f"Step {self.index} has 'sleep_ms' and cannot also have 'arg'"
                )
            if self._wait_for_completion is not None:
                raise ModelException(
                    f"Step {self.index} has 'sleep_ms' and cannot also have 'wait_for_completion'"
                )
            if self.sleep_ms is not None and not (
                0 <= self.sleep_ms <= self.MAX_STATIC_SLEEP_MS
            ):
                raise ModelException(
                    f"Step {self.index} sleep_ms {self.sleep_ms} is out of "
                    f"range 0 .. {self.MAX_STATIC_SLEEP_MS}"
                )
            return
        # Command-form parenthesis sanity.
        if self.arg:
            if self.arg.count("(") != self.arg.count(")"):
                raise ModelException(
                    f"Mismatched parentheses in arg expression for step {self.index}: {self.arg}"
                )

    def resolve_arg_type(self, command_obj):
        """
        Derive the arg type package from the resolved command object so the
        template can emit the correct Serialization.To_Byte_Array call.
        Only used for static (non-dynamic) arg steps.
        """
        if not self.arg:
            return
        arg_type = getattr(command_obj, "datatype", None)
        self.arg_type_package = arg_type.package

    def resolve_dynamic_arg_type(self, command_obj, parent_sequence):
        """
        For dynamic steps, resolve:
          - input_type_package: the Ada package of the sequence's arg_type
            (the root "Arg" in "Arg.A.B.C.D")
          - traversal_path: the dotted path after "Arg." e.g. "A.B.C.D"
          - dynamic_arg_type_package: the Ada package of the leaf field type,
            derived from the command's argument datatype (same as static arg_type_package)

        The generator trusts that the traversal path is valid — Ada will reject
        the generated code at compile time if it isn't.
        """
        if not self.dynamic_arg:
            return

        # The sequence must have an arg_type for dynamic steps to draw from
        if not parent_sequence.arg_type_package:
            raise ModelException(
                f"Step {self.index} has dynamic_arg '{self.dynamic_arg}' but "
                f"sequence '{parent_sequence.name}' has no arg_type defined"
            )

        self.input_type_package = parent_sequence.arg_type_package

        # Strip the leading "Arg." to get the field traversal path.
        # For bare "Arg" there is no traversal — the root type is the leaf.
        self.traversal_path = self.dynamic_arg[len("Arg."):] if "." in self.dynamic_arg else None

        # The leaf type is the command's argument datatype — same resolution
        # as for static args
        arg_type = getattr(command_obj, "datatype", None)
        if arg_type is None:
            raise ModelException(
                f"Step {self.index} has dynamic_arg but command "
                f"'{self.command}' has no argument type"
            )
        self.dynamic_arg_type_package = arg_type.package

        # Resolver type/instance names are scoped to sequence + step index so
        # that two steps in the same sequence targeting the same command never
        # collide (e.g. Sequence_B_Step_0_Resolver_T vs _Step_1_Resolver_T).
        self.resolver_type_name = (
            f"{parent_sequence.name}_Step_{self.index}_Resolver_T"
        )
        self.resolver_instance_name = (
            f"{parent_sequence.name}_Step_{self.index}_Resolver"
        )

    def resolve_dynamic_sleep(self, parent_sequence):
        """
        For dynamic sleep steps, resolve the same fields as
        resolve_dynamic_arg_type, except the leaf type is fixed: a dynamic
        sleep always resolves its duration as a Packed_U32 millisecond count,
        so no command object is involved.
        """
        if not self.dynamic_sleep_arg:
            return

        if not parent_sequence.arg_type_package:
            raise ModelException(
                f"Step {self.index} has dynamic sleep_ms "
                f"'{self.dynamic_sleep_arg}' but sequence "
                f"'{parent_sequence.name}' has no arg_type defined"
            )

        self.input_type_package = parent_sequence.arg_type_package
        self.traversal_path = (
            self.dynamic_sleep_arg[len("Arg."):]
            if "." in self.dynamic_sleep_arg
            else None
        )
        self.dynamic_arg_type_package = "Packed_U32"
        self.resolver_type_name = (
            f"{parent_sequence.name}_Step_{self.index}_Resolver_T"
        )
        self.resolver_instance_name = (
            f"{parent_sequence.name}_Step_{self.index}_Resolver"
        )

    def get_arg_expression(self):
        """Replace bare 'Arg' references with 'Sequence_Arg' in the command arg expression."""
        if not self.arg:
            return None
        return re.sub(r'\bArg\b', 'Sequence_Arg', self.arg)

    def get_sleep_expression(self):
        """Render the static sleep duration (a plain Natural) for the step table."""
        if self.sleep_ms is None:
            return None
        return str(self.sleep_ms)

    def has_arg(self):
        return self.arg is not None

    def is_dynamic(self):
        return self.dynamic_arg is not None

    def is_sleep(self):
        return self.sleep_ms is not None or self.dynamic_sleep_arg is not None

    def is_static_sleep(self):
        return self.sleep_ms is not None

    def is_dynamic_sleep(self):
        return self.dynamic_sleep_arg is not None

    def needs_resolver(self):
        """True for any step whose value is resolved from the sequence's
        per-call argument at execution time (dynamic command arg or dynamic
        sleep) -- these each get a generated Resolver function."""
        return self.is_dynamic() or self.is_dynamic_sleep()

    @classmethod
    @throw_exception_with_lineno
    def from_step_data(cls, step_data):
        command = step_data.get("command", None)
        arg = step_data.get("arg", None)
        wait_for_completion = step_data.get("wait_for_completion", None)
        sleep_ms = step_data.get("sleep_ms", None)
        return cls(
            command=command,
            arg=arg,
            wait_for_completion=wait_for_completion,
            sleep_ms=sleep_ms,
        )


class command_sequence(command):
    """
    Represents a single command sequence definition.
    """

    # Allowed values for the per-sequence response_behavior field, as Ada
    # enumeration literals of Sequence_Enums.Sequence_Response_Behavior.E.
    RESPONSE_BEHAVIORS = ("Send_After_Sequence_Start", "Send_After_Sequence_Completion")

    def __init__(
        self,
        name,
        sequence_steps,
        id=None,
        description=None,
        arg_type=None,
        wait_for_command_completion=True,
        continue_on_failure=False,
        command_timeout_seconds=None,
        response_behavior=None,
        suite=None,
    ):
        self.name = name
        self.description = description
        self.arg_type = arg_type
        self.wait_for_command_completion = wait_for_command_completion
        self.continue_on_failure = continue_on_failure
        self._command_timeout_seconds = command_timeout_seconds
        self.suite = suite
        self.steps = sequence_steps

        # When the sequencer replies to this sequence's own command: immediately
        # on start (the default) or deferred until the sequence completes,
        # carrying its final success/failure. Static per-sequence configuration,
        # baked into the generated Sequences_Table. Invocations via the generic
        # Run_Sequence command choose their behavior per-call instead.
        if response_behavior is None:
            self.response_behavior = "Send_After_Sequence_Start"
        else:
            formatted = ada.formatType(str(response_behavior))
            if formatted not in self.RESPONSE_BEHAVIORS:
                raise ModelException(
                    f"Sequence '{name}' has invalid response_behavior "
                    f"'{response_behavior}'. Must be one of: "
                    + ", ".join(b.lower() for b in self.RESPONSE_BEHAVIORS)
                )
            self.response_behavior = formatted

        if not re.match(r'^[A-Za-z][A-Za-z0-9_]*$', self.name):
            raise ModelException(
                f"Sequence name '{self.name}' must start with a letter and "
                "contain only letters, numbers, and underscores"
            )

        self.arg_type_model = None
        self.arg_type_package = None
        self.arg_type_name = None

        if self.arg_type:
            # The generated builders and resolvers reference the arg type's
            # package children (Serialization, Validation), so the type must be
            # package-qualified.
            if "." not in self.arg_type:
                raise ModelException(
                    f"Sequence '{self.name}' arg_type '{self.arg_type}' must be "
                    "a package-qualified type name (e.g. 'My_Args.T')"
                )
            parts = self.arg_type.rsplit(".", 1)
            self.arg_type_package = parts[0]
            self.arg_type_name = parts[1]

        # The generated step tables are indexed by Interfaces.Unsigned_16 and
        # the step counter must be able to advance one past the last index.
        if len(self.steps) > 65535:
            raise ModelException(
                f"Sequence '{self.name}' has {len(self.steps)} steps; at most "
                "65535 are supported"
            )

        for idx, step in enumerate(self.steps):
            step.index = idx
            step.set_defaults(self)
            step.validate()

            if step.arg and "Arg" in step.arg and not self.arg_type:
                raise ModelException(
                    f"Step {idx} references 'Arg' but sequence '{self.name}' "
                    "has no arg_type defined"
                )
            if step.is_dynamic_sleep() and not self.arg_type:
                raise ModelException(
                    f"Step {idx} has dynamic sleep_ms "
                    f"'{step.dynamic_sleep_arg}' but sequence '{self.name}' "
                    "has no arg_type defined"
                )
        # The command's wire/arg type is the sequence's own arg_type (or none
        # for an argless sequence) -- a user-written, normally-registered type.
        # Response behavior is static per-sequence configuration in the
        # generated Sequences_Table, so nothing is appended to the wire type
        # and no record generation is involved.
        super(command_sequence, self).__init__(
            name, type=self.arg_type, description=description, id=id, suite=suite
        )

    def get_command_name(self):
        return self.name

    def has_arg(self):
        return self.arg_type is not None

    def has_dynamic_steps(self):
        return any(step.needs_resolver() for step in self.steps)

    @property
    def command_timeout_millis(self):
        seconds = self._command_timeout_seconds
        if seconds is None and self.suite is not None:
            seconds = getattr(self.suite, "command_timeout_seconds", None)
        if seconds is None:
            seconds = DEFAULT_COMMAND_TIMEOUT_SECONDS
        return seconds * 1000

    @classmethod
    @throw_exception_with_lineno
    def from_sequence_data(cls, seq_data, suite=None):
        name = seq_data["name"]
        description = seq_data.get("description", None)
        wait_for_command_completion = seq_data.get("wait_for_command_completion", True)
        continue_on_failure = seq_data.get("continue_on_failure", False)
        command_timeout_seconds = seq_data.get("command_timeout_seconds", None)
        response_behavior = seq_data.get("response_behavior", None)
        arg_type = seq_data.get("arg_type", None)

        sequence_steps = []
        if "sequence" not in seq_data or not seq_data["sequence"]:
            raise ModelException(f"Sequence '{name}' has no steps defined")

        for step_data in seq_data["sequence"]:
            sequence_steps.append(sequence_step.from_step_data(step_data))

        return cls(
            name=name,
            sequence_steps=sequence_steps,
            description=description,
            arg_type=arg_type,
            wait_for_command_completion=wait_for_command_completion,
            continue_on_failure=continue_on_failure,
            command_timeout_seconds=command_timeout_seconds,
            response_behavior=response_behavior,
            suite=suite,
        )


class command_sequences(assembly_submodel):
    """
    Object model for command sequences.
    """

    def __init__(self, filename):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        schema_dir = os.path.join(this_file_dir, ".." + os.sep + "schemas")
        super(command_sequences, self).__init__(
            filename, schema_dir + "/command_sequences.yaml"
        )

    def load(self):
        self.name = None
        self.description = None
        self.preamble = None
        self.command_timeout_seconds = None
        self.includes = []
        self.sequences = OrderedDict()
        self.sequence_names = []

        self.suite_name = None
        self.assembly_name = None

        self.name = ada.formatType(self.model_name) + "_Command_Sequences"
        if self.specific_name:
            self.name = self.name + "_" + ada.formatVariable(self.specific_name)

        if "name" in self.data:
            self.suite_name = self.data["name"]

        if "description" in self.data:
            self.description = self.data["description"]

        if "preamble" in self.data:
            self.preamble = self.data["preamble"]

        if "command_timeout_seconds" in self.data:
            self.command_timeout_seconds = self.data["command_timeout_seconds"]

        if "with" in self.data:
            self.includes = self.data["with"]
            for include in self.includes:
                include = ada.formatType(include)
            self.includes = list(set(self.includes))
            # The generated spec always carries "with Sequence_Enums;" (the
            # per-sequence Response_Behavior configuration in the
            # Sequences_Table), so drop it from user includes to avoid a
            # duplicate with clause.
            self.includes = [inc for inc in self.includes if inc != "Sequence_Enums"]

        if "sequences" not in self.data or not self.data["sequences"]:
            raise ModelException("At least one sequence must be defined")

        for seq_data in self.data["sequences"]:
            seq = command_sequence.from_sequence_data(seq_data, suite=self)
            seq.lineno = seq_data.lc.line

            if seq.name not in self.sequences:
                self.sequences[seq.name] = seq
                self.sequence_names.append(seq.name)
            else:
                raise ModelException(
                    f'Duplicate sequence name found: "{seq.name}"',
                    lineno=seq.lineno,
                )

        # All sequences are now in place; populate template-context flags.
        self._compute_template_flags()

    def has_dynamic_steps(self):
        """True if any sequence in this suite has at least one dynamic step."""
        return any(seq.has_dynamic_steps() for seq in self.sequences.values())

    def _compute_template_flags(self):
        """Compute boolean flags used by the templates as plain instance
        attributes so Jinja can reference them directly via the model's
        __dict__ render context. Call after self.sequences is populated."""
        # True if any sequence in this suite has at least one dynamic step
        # (drives Resolver type emission in name.ads).
        self.suite_has_dynamic_steps = self.has_dynamic_steps()
        # True if any step in any sequence needs the To_Arg helper: static
        # command arg expressions use it in the spec's step arrays, and
        # resolver-backed steps use it in the body's Resolver functions.
        self.needs_to_arg = any(
            step.has_arg() or step.needs_resolver()
            for seq in self.sequences.values()
            for step in seq.steps
        )
        # Arg type packages needed by the split-out command builders package
        # (name_commands.ads/adb) -- the builders take each sequence's native
        # arg type and serialize it.
        self.builder_includes = sorted(
            {
                seq.arg_type_package
                for seq in self.sequences.values()
                if seq.arg_type_package
            }
        )

    # Resolution errors raised here (unknown component, unknown command, bad
    # dynamic arg) happen outside the base class's load path, so attach the
    # yaml filename to them explicitly -- load-time errors get it from the
    # base class already.
    @throw_exception_with_filename
    def final(self):
        # Used by name.ads to `with` the assembly's command-id package
        # (<Assembly>_Commands). self.assembly is set by the base set_assembly,
        # which runs before final(); assembly_name was left None at load() time.
        self.assembly_name = self.assembly.name

        for seq in self.sequences.values():
            # The generated builder surface takes each sequence's native arg
            # type directly in the spec, so its package always needs a with
            # clause, whether or not any step traverses it dynamically.
            if seq.arg_type_package and seq.arg_type_package not in self.includes:
                self.includes.append(seq.arg_type_package)
            for step in seq.steps:
                # Sleep steps don't reference any assembly component or
                # command, so skip command resolution for them. A dynamic
                # sleep still needs its Resolver fields populated (leaf type
                # fixed at Packed_U32) so the templates can emit its resolver.
                if step.is_dynamic_sleep():
                    step.resolve_dynamic_sleep(seq)
                    if "Packed_U32" not in self.includes:
                        self.includes.append("Packed_U32")
                    continue
                if step.is_sleep():
                    continue
                comp = self.assembly.get_component_with_name(step.component_name)
                if not comp:
                    raise ModelException(
                        f'Sequence "{seq.name}" references component '
                        f'"{step.component_name}" which does not exist in assembly '
                        f'"{self.assembly.name}"',
                        lineno=seq.lineno,
                    )

                step.component = comp

                if not comp.commands:
                    raise ModelException(
                        f'Sequence "{seq.name}" references command '
                        f'"{step.command}", but component "{step.component_name}" '
                        f'does not have any commands',
                        lineno=seq.lineno,
                    )

                if step.command_name not in comp.commands.names():
                    raise ModelException(
                        f'Sequence "{seq.name}" references command '
                        f'"{step.command}", but component "{step.component_name}" '
                        f'does not have a command named "{step.command_name}". '
                        f'Available commands: {list(comp.commands.names())}',
                        lineno=seq.lineno,
                    )
                step.command_obj = comp.commands.get_with_name(step.command_name)

                # A sequence configured with send_after_sequence_completion may
                # not invoke itself as a sub-sequence: the outer run's deferred
                # reply waits on the inner run, which starts another copy of the
                # same sequence, recursively occupying frames until none are
                # free and the innermost dispatch fails. The configuration can
                # never succeed, so reject it at model time. The target counts
                # as "self" when it is a Simple_Command_Sequencer instance
                # initialized with THIS suite and the invoked command name is
                # the containing sequence's own name.
                if (
                    seq.response_behavior == "Send_After_Sequence_Completion"
                    and step.command_name.lower() == seq.name.lower()
                    and comp.name == "Simple_Command_Sequencer"
                ):
                    sequences_value = comp.init.get_parameter_value("Sequences")
                    if (
                        sequences_value
                        and sequences_value.split(".")[0].lower() == self.name.lower()
                    ):
                        raise ModelException(
                            f'Sequence "{seq.name}" has response_behavior '
                            f'send_after_sequence_completion and calls itself via '
                            f'"{step.command}" (step {step.index}). A deferred-'
                            f'completion sequence may not invoke itself: each run '
                            f'would wait on a new copy of the same sequence, '
                            f'consuming frames until dispatch fails. Remove the '
                            f'self-call or use send_after_sequence_start.',
                            lineno=seq.lineno,
                        )

                # Resolve arg type — static and dynamic are mutually exclusive
                if step.is_dynamic():
                    step.resolve_dynamic_arg_type(step.command_obj, seq)
                    # Auto-populate includes for the leaf arg type so the
                    # generated code gets the correct "with" clauses (the
                    # sequence-level input type is already included above).
                    if step.dynamic_arg_type_package and step.dynamic_arg_type_package not in self.includes:
                        self.includes.append(step.dynamic_arg_type_package)
                else:
                    step.resolve_arg_type(step.command_obj)
                    if step.arg_type_package and step.arg_type_package not in self.includes:
                        self.includes.append(step.arg_type_package)

                self.dependencies.extend(
                    [comp.commands.full_filename] + comp.commands.get_dependencies()
                )

        self.dependencies = list(set(self.dependencies))
        # Final dedup of includes preserving order
        seen = set()
        deduped = []
        for inc in self.includes:
            if inc and inc not in seen:
                seen.add(inc)
                deduped.append(inc)
        self.includes = deduped

        self._check_engine_connection_counts()

    def _check_engine_connection_counts(self):
        """
        Each sequencer frame (engine) is claimed via a Register_Source reply
        routed back on Command_Response_T_Recv_Async -- typically one arrayed
        command-router connector entry per engine. A frame that never receives
        one is a phantom: it can never be claimed. The component cannot see
        its inbound connection count at runtime, but the assembly knows it at
        generation time, so warn when it differs from the instance's
        configured engine count.
        """
        for comp in self.assembly.components.values():
            if comp.name != "Simple_Command_Sequencer" or not comp.init:
                continue
            sequences_value = comp.init.get_parameter_value("Sequences")
            if (
                not sequences_value
                or sequences_value.split(".")[0].lower() != self.name.lower()
            ):
                continue
            num_engines_value = comp.init.get_parameter_value(
                "Num_Concurrent_Sequences"
            )
            try:
                num_engines = int(str(num_engines_value).strip())
            except (TypeError, ValueError):
                # The engine count is a non-literal expression; it cannot be
                # checked at generation time.
                continue
            num_connections = sum(
                1
                for conn in self.assembly.connections
                if conn.to_component is comp
                and conn.to_connector is not None
                and conn.to_connector.name == "Command_Response_T_Recv_Async"
            )
            if num_connections != num_engines:
                self.warn(
                    f"component '{comp.instance_name}' is configured with "
                    f"Num_Concurrent_Sequences => {num_engines} but has "
                    f"{num_connections} connection(s) into "
                    "Command_Response_T_Recv_Async. Each engine needs its own "
                    "inbound command-response connection (one command-router "
                    "arrayed connector entry per engine) to receive a "
                    "Register_Source reply; engines without one can never be "
                    "claimed."
                )

    def load_type(self, type_name):
        return model_loader.try_load_model_by_name(type_name, model_types="type")
