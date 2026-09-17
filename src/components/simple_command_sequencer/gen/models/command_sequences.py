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
from models.simple_command_sequencer_packets import get_max_frames_per_packet
import re

DEFAULT_COMMAND_TIMEOUT_MS = 30000


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
        sleep_ms=None,
    ):
        # A step is either a command dispatch or a sleep, never both. The
        # parser/validator enforces this so downstream code can branch on
        # is_sleep() / is_command().
        self.command = command
        # sleep_ms is either a static integer millisecond count or a dynamic
        # reference into the sequence's argument ("Arg" or "Arg.A.B"), resolved
        # at execution time as a Packed_Natural.
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
        # Sleep-form: no arg field allowed, and static durations must fit a
        # Natural (and thus an Ada.Real_Time.Time_Span).
        if self.is_sleep():
            if self.arg is not None or self.dynamic_arg is not None:
                raise ModelException(
                    f"Step {self.index} has 'sleep_ms' and cannot also have 'arg'"
                )
            if self.sleep_ms is not None and not (
                0 <= self.sleep_ms <= self.MAX_STATIC_SLEEP_MS
            ):
                raise ModelException(
                    f"Step {self.index} sleep_ms {self.sleep_ms} is out of "
                    f"range 0 .. {self.MAX_STATIC_SLEEP_MS}"
                )
            return
        # A static arg is baked into the step table at build time, so it cannot
        # carry a per-call value: any mention of Arg outside a pure reference
        # would be emitted verbatim and fail to compile in the generated
        # package, far from the yaml that caused it.
        if self.arg and re.search(r"\bArg\b", self.arg):
            raise ModelException(
                f"Step {self.index} arg '{self.arg}' mixes a reference to the "
                "sequence argument with other Ada. A step arg must be either "
                "exactly 'Arg' or a field path 'Arg.Field[.Subfield]', or a "
                "static Ada expression that does not mention Arg."
            )
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
        self.resolver_type_name = self._resolver_name(parent_sequence)

    def resolve_dynamic_sleep(self, parent_sequence):
        """
        Like resolve_dynamic_arg_type, but the leaf type is fixed: a dynamic
        sleep resolves its duration as a Packed_Natural millisecond count, so
        it always fits an Ada.Real_Time.Time_Span.
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
        self.dynamic_arg_type_package = "Packed_Natural"
        self.resolver_type_name = self._resolver_name(parent_sequence)

    def _resolver_name(self, parent_sequence):
        # Scoped to sequence and step index so two steps of one sequence
        # targeting the same command never collide.
        return f"{parent_sequence.name}_Step_{self.index}_Resolver_T"

    def get_arg_expression(self):
        """The static Ada expression baked into the step table, or None."""
        return self.arg or None

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
        if "wait_for_completion" in step_data:
            raise ModelException(
                "Step has 'wait_for_completion', but the sequencer waits for "
                "sub-command responses per sequence, not per step: set "
                "wait_for_command_completion on the sequence instead."
            )
        command = step_data.get("command", None)
        arg = step_data.get("arg", None)
        sleep_ms = step_data.get("sleep_ms", None)
        return cls(
            command=command,
            arg=arg,
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
        continue_on_failure=None,
        command_timeout_ms=None,
        response_behavior=None,
        suite=None,
    ):
        self.name = name
        self.description = description
        self.arg_type = arg_type
        self.wait_for_command_completion = wait_for_command_completion
        # None means the key was omitted: the sequence aborts on failure, but
        # the author promised nothing.
        self.continue_on_failure = bool(continue_on_failure)
        self._continue_on_failure_explicit = continue_on_failure is not None
        self._command_timeout_ms = command_timeout_ms
        self.suite = suite
        self.steps = sequence_steps

        # When the sequencer replies to this sequence's command: on start (the
        # default) or on completion, carrying the final success/failure. Baked
        # into the generated Sequences_Table.
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
            # Generated code references the type's Serialization and Validation
            # children, so it must be package-qualified.
            if "." not in self.arg_type:
                raise ModelException(
                    f"Sequence '{self.name}' arg_type '{self.arg_type}' must be "
                    "a package-qualified type name (e.g. 'My_Args.T')"
                )
            parts = self.arg_type.rsplit(".", 1)
            self.arg_type_package = parts[0]
            self.arg_type_name = parts[1]

        # Step tables are indexed by Unsigned_16; the counter must advance one
        # past the last index.
        if len(self.steps) > 65535:
            raise ModelException(
                f"Sequence '{self.name}' has {len(self.steps)} steps; at most "
                "65535 are supported"
            )

        for idx, step in enumerate(self.steps):
            step.index = idx
            step.validate()

            if step.is_dynamic() and not self.arg_type:
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

        # A sub-command failure is only ever seen when the sequence waits for
        # its responses, so on a no-wait sequence continue_on_failure can never
        # abort anything. Refuse an explicit false there rather than let the
        # yaml promise an abort the sequencer cannot deliver. A sequence with
        # no command steps has nothing to fail and is left alone.
        if (
            self._continue_on_failure_explicit
            and not self.continue_on_failure
            and not self.wait_for_command_completion
            and any(step.command is not None for step in self.steps)
        ):
            raise ModelException(
                f"Sequence '{self.name}' sets continue_on_failure: false but "
                "wait_for_command_completion: false, so no sub-command failure "
                "can ever be seen and the sequence could never abort. Remove "
                "continue_on_failure, or wait for command completion."
            )
        # The command's arg type is the sequence's own arg_type (or none), a
        # user-written, normally-registered type; nothing is generated for it.
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
        millis = self._command_timeout_ms
        if millis is None and self.suite is not None:
            millis = getattr(self.suite, "command_timeout_ms", None)
        if millis is None:
            millis = DEFAULT_COMMAND_TIMEOUT_MS
        return millis

    @classmethod
    @throw_exception_with_lineno
    def from_sequence_data(cls, seq_data, suite=None):
        name = seq_data["name"]
        description = seq_data.get("description", None)
        wait_for_command_completion = seq_data.get("wait_for_command_completion", True)
        continue_on_failure = seq_data.get("continue_on_failure", None)
        command_timeout_ms = seq_data.get("command_timeout_ms", None)
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
            command_timeout_ms=command_timeout_ms,
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
        self.num_waiting_frames = None
        self.num_non_waiting_frames = None
        self.num_frames = None
        self.command_timeout_ms = None
        self.includes = []
        self.sequences = OrderedDict()

        self.assembly_name = None

        self.name = ada.formatType(self.model_name) + "_Command_Sequences"
        if self.specific_name:
            self.name = self.name + "_" + ada.formatVariable(self.specific_name)

        if "description" in self.data:
            self.description = self.data["description"]

        if "preamble" in self.data:
            self.preamble = self.data["preamble"]

        # The suite owns the two frame pools: sequences that wait for their
        # sub-command responses run on one, non-waiting sequences on the other, so
        # a late response to a finished non-waiting run can never reach a frame that
        # is parked waiting. Together the pools size the summary packet type, so
        # check the total fits one packet buffer at model load.
        self.num_waiting_frames = self.data["num_waiting_frames"]
        self.num_non_waiting_frames = self.data["num_non_waiting_frames"]
        self.num_frames = self.num_waiting_frames + self.num_non_waiting_frames
        max_frames = get_max_frames_per_packet()
        if self.num_frames == 0:
            raise ModelException(
                "num_waiting_frames and num_non_waiting_frames are both 0; a "
                "sequencer needs at least one frame."
            )
        if self.num_frames > max_frames:
            raise ModelException(
                f"num_waiting_frames + num_non_waiting_frames is {self.num_frames} "
                f"but at most {max_frames} Sequence_Frame_Summary entries fit "
                "in one summary packet with the project's configured packet "
                "buffer size."
            )

        if "command_timeout_ms" in self.data:
            self.command_timeout_ms = self.data["command_timeout_ms"]

        if "with" in self.data:
            # The generated spec already has "with Sequence_Enums;"; drop it from user includes.
            self.includes = [
                inc
                for inc in dict.fromkeys(ada.formatType(w) for w in self.data["with"])
                if inc != "Sequence_Enums"
            ]

        if "sequences" not in self.data or not self.data["sequences"]:
            raise ModelException("At least one sequence must be defined")

        for seq_data in self.data["sequences"]:
            seq = command_sequence.from_sequence_data(seq_data, suite=self)
            seq.lineno = seq_data.lc.line

            if seq.name not in self.sequences:
                self.sequences[seq.name] = seq
            else:
                raise ModelException(
                    f'Duplicate sequence name found: "{seq.name}"',
                    lineno=seq.lineno,
                )

        # The generated suite and everything the assembly derives from the
        # injected commands change with the argument types, so depend on them.
        for seq in self.sequences.values():
            if seq.type_model is not None:
                self.dependencies.append(seq.type_model.full_filename)
                self.dependencies.extend(seq.type_model.get_dependencies())
        self.dependencies = list(dict.fromkeys(self.dependencies))

        self._check_pool_sizes()

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

    # Errors raised here are outside the base class's load path, so attach the
    # yaml filename explicitly.
    @throw_exception_with_filename
    def final(self):
        # Used by name.ads to `with` the assembly's command-id package
        # (<Assembly>_Commands). self.assembly is set by the base set_assembly,
        # which runs before final(); assembly_name was left None at load() time.
        self.assembly_name = self.assembly.name

        # Sequence-to-sequence calls, for the deferred-call cycle check below.
        sequences_by_lower_name = {name.lower(): s for name, s in self.sequences.items()}
        calls = {name: [] for name in self.sequences}

        for seq in self.sequences.values():
            # The generated builder surface takes each sequence's native arg
            # type directly in the spec, so its package always needs a with
            # clause, whether or not any step traverses it dynamically.
            if seq.arg_type_package and seq.arg_type_package not in self.includes:
                self.includes.append(seq.arg_type_package)
            for step in seq.steps:
                # Sleep steps reference no assembly command. A dynamic sleep
                # still needs its Resolver fields so the templates can emit it.
                if step.is_dynamic_sleep():
                    step.resolve_dynamic_sleep(seq)
                    if "Packed_Natural" not in self.includes:
                        self.includes.append("Packed_Natural")
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

                # A step that targets a sequence of this suite, through a
                # sequencer instance initialized with it, is an edge in the
                # suite's call graph -- but only when this sequence waits for
                # the call's response, since a no-wait caller's frame is never
                # held by the callee.
                if comp.name == "Simple_Command_Sequencer" and seq.wait_for_command_completion:
                    config_value = comp.init.get_parameter_value("Config")
                    callee = sequences_by_lower_name.get(step.command_name.lower())
                    if (
                        callee is not None
                        and config_value
                        and config_value.split(".")[0].lower() == self.name.lower()
                    ):
                        calls[seq.name].append((callee.name, step))

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

        self._check_deferred_call_cycles(calls)
        self._check_engine_connection_counts()

    def _check_deferred_call_cycles(self, calls):
        """
        Reject a cycle of sequence calls in which every sequence replies on
        completion and every caller waits for its steps' responses. A waiting
        call into a send_after_sequence_completion sequence parks the caller's
        frame until the callee finishes, so around such a cycle no run can ever
        finish: each waits on a fresh run of the next, claiming frames until
        dispatch fails. A cycle that passes through a send_after_sequence_start
        sequence, or through a caller that does not wait, completes one run per
        lap and is left alone; a sequence may loop through itself that way on
        purpose.
        """
        deferred = {
            name
            for name, seq in self.sequences.items()
            if seq.response_behavior == "Send_After_Sequence_Completion"
        }
        # Depth-first search over the deferred sequences only; a back edge to a
        # sequence still on the path closes a cycle.
        on_path = []
        finished = set()

        def visit(name):
            on_path.append(name)
            for callee, step in calls[name]:
                if callee not in deferred:
                    continue
                if callee in on_path:
                    cycle = on_path[on_path.index(callee):] + [callee]
                    raise ModelException(
                        f'Sequence "{name}" calls "{callee}" via "{step.command}" '
                        f'(step {step.index}), closing the cycle '
                        + " -> ".join(cycle)
                        + " in which every sequence has response_behavior "
                        "send_after_sequence_completion and waits for command "
                        "completion. Each run would wait on a new run of the next "
                        "sequence, so none could finish and frames would be "
                        "consumed until dispatch fails. Break the cycle, give one "
                        "of these sequences send_after_sequence_start, or let one "
                        "of them run without waiting for command completion.",
                        lineno=self.sequences[name].lineno,
                    )
                if callee not in finished:
                    visit(callee)
            on_path.pop()
            finished.add(name)

        for name in deferred:
            if name not in finished:
                visit(name)

    def _check_pool_sizes(self):
        """
        A sequence draws its frame from the pool matching its
        wait_for_command_completion setting, so a suite that declares a
        sequence of one kind must give that pool at least one frame; otherwise
        the sequence could never be started.
        """
        for seq in self.sequences.values():
            if seq.wait_for_command_completion and self.num_waiting_frames == 0:
                raise ModelException(
                    f"Sequence '{seq.name}' waits for command completion but "
                    "num_waiting_frames is 0, so it could never claim a frame. "
                    "Give the waiting pool at least one frame or make the "
                    "sequence not wait.",
                    lineno=seq.lineno,
                )
            if not seq.wait_for_command_completion and self.num_non_waiting_frames == 0:
                raise ModelException(
                    f"Sequence '{seq.name}' does not wait for command completion "
                    "but num_non_waiting_frames is 0, so it could never claim a "
                    "frame. Give the non-waiting pool at least one frame or make the "
                    "sequence wait.",
                    lineno=seq.lineno,
                )

    def _check_engine_connection_counts(self):
        """
        Each sequencer frame (engine) is claimed via a Register_Source reply
        routed back on Command_Response_T_Recv_Async -- typically one arrayed
        command-router connector entry per engine. A frame that never receives
        one is a phantom: it can never be claimed. The component cannot see
        its inbound wiring at runtime, but the assembly knows it at generation
        time, so warn when the connection count differs from the instance's
        configured engine count.
        """
        for comp in self.assembly.components.values():
            if comp.name != "Simple_Command_Sequencer" or not comp.init:
                continue
            config_value = comp.init.get_parameter_value("Config")
            if (
                not config_value
                or config_value.split(".")[0].lower() != self.name.lower()
            ):
                continue
            num_engines = self.num_frames
            # On the invokee side each index of get_connections() is None,
            # "ignore", or the list of connections fanned into that index.
            inbound = []
            response_connector = comp.connectors.of_name(
                "Command_Response_T_Recv_Async"
            )
            for index_connections in response_connector.get_connections():
                if index_connections is not None and index_connections != "ignore":
                    inbound.extend(index_connections)
            if len(inbound) != num_engines:
                self.warn(
                    f"component '{comp.instance_name}' is configured with "
                    f"{num_engines} frames (num_waiting_frames + "
                    f"num_non_waiting_frames) but has "
                    f"{len(inbound)} connection(s) into "
                    "Command_Response_T_Recv_Async. Each engine needs its own "
                    "inbound command-response connection (one command-router "
                    "arrayed connector entry per engine) to receive a "
                    "Register_Source reply; engines without one can never be "
                    "claimed."
                )
