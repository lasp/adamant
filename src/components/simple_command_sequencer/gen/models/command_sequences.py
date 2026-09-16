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
from util import model_loader
import re

DEFAULT_COMMAND_TIMEOUT_MS = 30000

# The passthrough capacity for a sequence's argument, cached at module level.
_run_sequence_buffer_size_bytes = [None]


def get_max_sequence_arg_size_bytes():
    """
    The largest sequence argument, in bytes, that fits the Run_Sequence
    passthrough buffer -- the Python mirror of
    Simple_Sequencer_Types.Run_Sequence_Arg_Buffer_Length_Type'Last. Read from
    the Buffer_Arg field of the Run_Sequence_Arg record, which the project's
    configured command buffer size shapes, so the model and the Ada subtype
    cannot disagree.
    """
    if _run_sequence_buffer_size_bytes[0] is None:
        arg_model = model_loader.try_load_model_by_name(
            "Run_Sequence_Arg", model_types="record"
        )
        if not arg_model:
            raise ModelException(
                "Could not load model for Run_Sequence_Arg.T. This must be in the path."
            )
        for fld in arg_model.fields.values():
            if fld.name == "Buffer_Arg":
                _run_sequence_buffer_size_bytes[0] = int(fld.size / 8)
                break
        else:
            assert False, "No field 'Buffer_Arg' found in Run_Sequence_Arg.T type"
    return _run_sequence_buffer_size_bytes[0]


class sequence_step(object):
    """
    Represents a single step in a command sequence.
    """

    # A dynamic value references the sequence's per-call argument as "Arg".
    # _DYNAMIC_ARG_RE matches the pure path form, "Arg" or "Arg.Field.Sub",
    # which forwards one field; _ARG_TOKEN_RE finds any reference, so a larger
    # Ada expression embedding one is dynamic too. An arg with no reference is
    # a static Ada expression serialized at build time.
    _DYNAMIC_ARG_RE = re.compile(r'^Arg(\.[A-Za-z][A-Za-z0-9_]*)*$')
    _ARG_TOKEN_RE = re.compile(r'\bArg\b')

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
        # An arg that references the sequence argument is stored in dynamic_arg
        # and arg is cleared; otherwise it stays in arg.
        if arg is not None and self._ARG_TOKEN_RE.search(arg):
            self.arg = None
            self.dynamic_arg = arg.strip()
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
        #   resolver_expression - the Ada expression the Resolver serializes,
        #                         with the deserialized argument as "Input",
        #                         e.g. "Input.A.B" for "Arg.A.B" or
        #                         "(Mode => Input.Mode, Enable => True)"
        #   dynamic_arg_type_package - Ada package of the leaf field type
        #                         e.g. "Sys_Time_32"
        self.input_type_package = None
        self.resolver_expression = None
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
        expression = self.arg or self.dynamic_arg
        if expression and expression.count("(") != expression.count(")"):
            raise ModelException(
                f"Mismatched parentheses in arg expression for step {self.index}: {expression}"
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
            (what "Arg" refers to)
          - resolver_expression: the step's expression with every "Arg"
            reference rebound to the Resolver's deserialized "Input"
          - dynamic_arg_type_package: the Ada package of the leaf field type,
            derived from the command's argument datatype (same as static arg_type_package)

        The generator trusts that the expression is valid — Ada will reject
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
        self.resolver_expression = self._ARG_TOKEN_RE.sub("Input", self.dynamic_arg)

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
        self.resolver_expression = self._ARG_TOKEN_RE.sub("Input", self.dynamic_sleep_arg)
        self.dynamic_arg_type_package = "Packed_Natural"
        self.resolver_type_name = (
            f"{parent_sequence.name}_Step_{self.index}_Resolver_T"
        )
        self.resolver_instance_name = (
            f"{parent_sequence.name}_Step_{self.index}_Resolver"
        )

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
        command_timeout_ms=None,
        response_timeout_ms=None,
        response_behavior=None,
        suite=None,
    ):
        self.name = name
        self.description = description
        self.arg_type = arg_type
        self.wait_for_command_completion = wait_for_command_completion
        self.continue_on_failure = continue_on_failure
        self._command_timeout_ms = command_timeout_ms
        self._response_timeout_ms = response_timeout_ms
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
            step.set_defaults(self)
            step.validate()

            if step.is_dynamic() and not self.arg_type:
                raise ModelException(
                    f"Step {idx} arg '{step.dynamic_arg}' references the sequence "
                    f"argument 'Arg' but sequence '{self.name}' has no arg_type "
                    "defined"
                )
            if step.is_dynamic_sleep() and not self.arg_type:
                raise ModelException(
                    f"Step {idx} has dynamic sleep_ms "
                    f"'{step.dynamic_sleep_arg}' but sequence '{self.name}' "
                    "has no arg_type defined"
                )

        # A no-wait step is fire-and-forget: its response arrives after the
        # frame has moved on and responses are matched by command id alone. A
        # later waiting step on the same command could therefore be woken
        # early by the no-wait step's stale response, so reject that shape.
        for idx, step in enumerate(self.steps):
            if step.command is None or step.wait_for_completion:
                continue
            for later in self.steps[idx + 1:]:
                if later.command == step.command and later.wait_for_completion:
                    raise ModelException(
                        f"Sequence '{self.name}': step {idx} dispatches "
                        f"'{step.command}' without waiting for completion, "
                        f"and step {later.index} waits on the same command. "
                        "The no-wait step's late response could wake the "
                        "waiting step early, since responses are matched by "
                        "command id. Reorder the steps, use a different "
                        "command, or make both steps wait."
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

    @property
    def response_timeout_millis(self):
        millis = self._response_timeout_ms
        if millis is None and self.suite is not None:
            millis = getattr(self.suite, "response_timeout_ms", None)
        if millis is None:
            # A response is expected within the command timeout, so silence
            # beyond one command timeout means it is lost.
            return self.command_timeout_millis
        return millis

    @classmethod
    @throw_exception_with_lineno
    def from_sequence_data(cls, seq_data, suite=None):
        name = seq_data["name"]
        description = seq_data.get("description", None)
        wait_for_command_completion = seq_data.get("wait_for_command_completion", True)
        continue_on_failure = seq_data.get("continue_on_failure", False)
        command_timeout_ms = seq_data.get("command_timeout_ms", None)
        response_timeout_ms = seq_data.get("response_timeout_ms", None)
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
            response_timeout_ms=response_timeout_ms,
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
        self.num_concurrent_sequences = None
        self.command_timeout_ms = None
        self.response_timeout_ms = None
        self.includes = []
        self.sequences = OrderedDict()
        self.sequence_names = []

        self.assembly_name = None

        self.name = ada.formatType(self.model_name) + "_Command_Sequences"
        if self.specific_name:
            self.name = self.name + "_" + ada.formatVariable(self.specific_name)

        if "description" in self.data:
            self.description = self.data["description"]

        if "preamble" in self.data:
            self.preamble = self.data["preamble"]

        # The suite owns the frame-pool size, which also sizes its summary packet
        # type, so check it fits one packet buffer at model load.
        self.num_concurrent_sequences = self.data["num_concurrent_sequences"]
        max_frames = get_max_frames_per_packet()
        if self.num_concurrent_sequences > max_frames:
            raise ModelException(
                f"num_concurrent_sequences is {self.num_concurrent_sequences} "
                f"but at most {max_frames} Sequence_Frame_Summary entries fit "
                "in one summary packet with the project's configured packet "
                "buffer size."
            )

        if "command_timeout_ms" in self.data:
            self.command_timeout_ms = self.data["command_timeout_ms"]

        if "response_timeout_ms" in self.data:
            self.response_timeout_ms = self.data["response_timeout_ms"]

        if "with" in self.data:
            self.includes = self.data["with"]
            for include in self.includes:
                include = ada.formatType(include)
            self.includes = list(set(self.includes))
            # The generated spec already has "with Sequence_Enums;"; drop it from user includes.
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

        # The generated suite and everything the assembly derives from the
        # injected commands change with the argument types, so depend on them.
        for seq in self.sequences.values():
            if seq.type_model is not None:
                self.dependencies.append(seq.type_model.full_filename)
                self.dependencies.extend(seq.type_model.get_dependencies())
        self.dependencies = list(dict.fromkeys(self.dependencies))

        # A sequence's argument travels inside the Run_Sequence command argument,
        # behind its Sequence_Id and Arg_Length header fields, so it has less room
        # than an ordinary command argument. The generated Sequences_Table stores
        # each argument's serialized length in the passthrough length subtype,
        # which would otherwise fail its range check at elaboration.
        max_arg_bytes = get_max_sequence_arg_size_bytes()
        for seq in self.sequences.values():
            if seq.type_model is not None:
                arg_bytes = (seq.type_model.size + 7) // 8
                if arg_bytes > max_arg_bytes:
                    raise ModelException(
                        f'Sequence "{seq.name}" arg_type "{seq.arg_type}" serializes '
                        f'to {arg_bytes} bytes, but a sequence argument may be at most '
                        f'{max_arg_bytes} bytes. It is carried inside the Run_Sequence '
                        f'command argument behind the Sequence_Id and Arg_Length '
                        f'header fields, so a type that fits an ordinary command '
                        f'argument can still be too large here.',
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

    # Errors raised here are outside the base class's load path, so attach the
    # yaml filename explicitly.
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

                # A send_after_sequence_completion sequence may not call itself: the outer
                # run's deferred reply waits on an inner run of the same sequence, recursing
                # until no frame is free. "Self" means a Simple_Command_Sequencer instance
                # initialized with THIS suite invoking the containing sequence's own name.
                if (
                    seq.response_behavior == "Send_After_Sequence_Completion"
                    and step.command_name.lower() == seq.name.lower()
                    and comp.name == "Simple_Command_Sequencer"
                ):
                    config_value = comp.init.get_parameter_value("Config")
                    if (
                        config_value
                        and config_value.split(".")[0].lower() == self.name.lower()
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
            num_engines = self.num_concurrent_sequences
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
                    f"Num_Concurrent_Sequences => {num_engines} but has "
                    f"{len(inbound)} connection(s) into "
                    "Command_Response_T_Recv_Async. Each engine needs its own "
                    "inbound command-response connection (one command-router "
                    "arrayed connector entry per engine) to receive a "
                    "Register_Source reply; engines without one can never be "
                    "claimed."
                )

    def load_type(self, type_name):
        return model_loader.try_load_model_by_name(type_name, model_types="type")
