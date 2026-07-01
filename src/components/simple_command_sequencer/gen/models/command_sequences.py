from util import ada
import os.path
from collections import OrderedDict
from models.exceptions import ModelException, throw_exception_with_lineno
from models.assembly import assembly_submodel
from models.commands import (
    command
)
from util import model_loader
from util import redo
from util import redo_arg
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

    def __init__(
        self,
        command=None,
        arg=None,
        wait_for_completion=None,
        sleep=None,
    ):
        # A step is either a command dispatch or a sleep, never both. The
        # parser/validator enforces this so downstream code can branch on
        # is_sleep() / is_command().
        self.command = command
        self.sleep_expr = sleep
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
        # Mutual exclusivity: exactly one of command/sleep_expr.
        if self.command is None and self.sleep_expr is None:
            raise ModelException(
                f"Step {self.index} must specify either 'command' or 'sleep'"
            )
        if self.command is not None and self.sleep_expr is not None:
            raise ModelException(
                f"Step {self.index} cannot specify both 'command' and 'sleep'"
            )
        # Sleep-form: no arg/wait_for_completion fields allowed.
        if self.is_sleep():
            if self.arg is not None or self.dynamic_arg is not None:
                raise ModelException(
                    f"Step {self.index} has 'sleep' and cannot also have 'arg'"
                )
            if self._wait_for_completion is not None:
                raise ModelException(
                    f"Step {self.index} has 'sleep' and cannot also have 'wait_for_completion'"
                )
            if self.sleep_expr.count("(") != self.sleep_expr.count(")"):
                raise ModelException(
                    f"Mismatched parentheses in sleep expression for step {self.index}: {self.sleep_expr}"
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

    def get_arg_expression(self):
        """Replace bare 'Arg' references with 'Sequence_Arg' in the command arg expression."""
        if not self.arg:
            return None
        return re.sub(r'\bArg\b', 'Sequence_Arg', self.arg)

    def get_sleep_expression(self):
        """Replace bare 'Arg' references with 'Sequence_Arg' in the sleep expression."""
        if not self.sleep_expr:
            return None
        return re.sub(r'\bArg\b', 'Sequence_Arg', self.sleep_expr)

    def has_arg(self):
        return self.arg is not None

    def is_dynamic(self):
        return self.dynamic_arg is not None

    def is_sleep(self):
        return self.sleep_expr is not None

    @classmethod
    @throw_exception_with_lineno
    def from_step_data(cls, step_data):
        command = step_data.get("command", None)
        arg = step_data.get("arg", None)
        wait_for_completion = step_data.get("wait_for_completion", None)
        sleep = step_data.get("sleep", None)
        return cls(
            command=command,
            arg=arg,
            wait_for_completion=wait_for_completion,
            sleep=sleep,
        )


class command_sequence(command):
    """
    Represents a single command sequence definition.
    """

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

        if not re.match(r'^[A-Za-z][A-Za-z0-9_]*$', self.name):
            raise ModelException(
                f"Sequence name '{self.name}' must start with a letter and "
                "contain only letters, numbers, and underscores"
            )

        self.arg_type_model = None
        self.arg_type_package = None
        self.arg_type_name = None

        if self.arg_type:
            if "." in self.arg_type:
                parts = self.arg_type.rsplit(".", 1)
                self.arg_type_package = parts[0]
                self.arg_type_name = parts[1]
            else:
                self.arg_type_name = self.arg_type

        for idx, step in enumerate(self.steps):
            step.index = idx
            step.set_defaults(self)
            step.validate()

            if step.arg and "Arg" in step.arg and not self.arg_type:
                raise ModelException(
                    f"Step {idx} references 'Arg' but sequence '{self.name}' "
                    "has no arg_type defined"
                )
        # The COMMAND's wire/arg type is always the generated wrapped record
        # (<Name>_Run_Arg.T = native user args + a trailing Response_Behavior).
        # self.arg_type stays the NATIVE arg type (or None) so dynamic-step
        # resolution (resolve_dynamic_arg_type -> arg_type_package) traverses the
        # user's own record, not the wrapper.
        super(command_sequence, self).__init__(
            name, type=(self.name + "_Run_Arg.T"), description=description, id=id, suite=suite
        )

    def get_command_name(self):
        return self.name

    def has_arg(self):
        return self.arg_type is not None

    def has_dynamic_steps(self):
        return any(step.is_dynamic() for step in self.steps)

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

        # arg_type here is the sequence's NATIVE user-arg type (the optional
        # `arg_type:` in the YAML), used for dynamic-step resolution. The
        # command's wire type (the wrapped <Name>_Run_Arg.T) is derived in
        # command_sequence.__init__ -- we must NOT require that generated record
        # to exist at model-construction time, since this model is built during
        # redo's global rule-computation pass (before any file is generated) and
        # the wrapped record is itself a generated file (circular dependency).
        native_arg_type = seq_data.get("arg_type", None)

        sequence_steps = []
        if "sequence" not in seq_data or not seq_data["sequence"]:
            raise ModelException(f"Sequence '{name}' has no steps defined")

        for step_data in seq_data["sequence"]:
            sequence_steps.append(sequence_step.from_step_data(step_data))

        return cls(
            name=name,
            sequence_steps=sequence_steps,
            description=description,
            arg_type=native_arg_type,
            wait_for_command_completion=wait_for_command_completion,
            continue_on_failure=continue_on_failure,
            command_timeout_seconds=command_timeout_seconds,
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

        if "sequences" not in self.data or not self.data["sequences"]:
            raise ModelException("At least one sequence must be defined")

        # Each per-sequence command's wire type is the generated <Name>_Run_Arg
        # record (produced by the wrapped_run_args generator). Build any that are
        # not yet on disk so that constructing the command_sequence objects below
        # -- which eagerly resolve "<Name>_Run_Arg.T" via the model loader -- can
        # load them. Mirrors how packed_type builds its generated .type_ranges.yaml
        # during load. Two things make this safe (no recursion / no DB-setup
        # breakage):
        #   * output_filename no longer constructs this model, so it is only
        #     loaded during real generation, never during redo's DB-setup pass.
        #   * the wrapped_run_args generator sets has_dependencies=False, so
        #     building a run_arg record never loads this model back -- otherwise
        #     this redo_ifchange would re-enter and recurse.
        # Gating on existence keeps this from spawning a redo-ifchange child on
        # every one of the many model loads in a build; incremental staleness is
        # handled by the normal dependency graph (the suite's deps_list carries
        # each resolved record as a dependency of the assembly).
        src_dir = redo_arg.get_src_dir(self.full_filename)
        run_arg_records = [
            os.path.join(
                src_dir, "build", "yaml",
                str(seq_data["name"]).lower() + "_run_arg.record.yaml",
            )
            for seq_data in self.data["sequences"]
            if isinstance(seq_data, dict) and "name" in seq_data
        ]
        missing = [p for p in run_arg_records if not os.path.isfile(p)]
        if missing:
            redo.redo_ifchange(missing)

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
        """Compute boolean flags used by name.ads as plain instance attributes
        so the Jinja template can reference them directly via the model's
        __dict__ render context. Call after self.sequences is populated."""
        # True if any sequence in this suite has at least one dynamic step
        # (drives Resolver type emission in name.ads).
        self.suite_has_dynamic_steps = self.has_dynamic_steps()
        # True if any step in any sequence has a static command arg expression
        # (drives `with Sequence_Arg_Utils;` in name.ads).
        self.needs_sequence_arg_utils = any(
            step.has_arg()
            for seq in self.sequences.values()
            for step in seq.steps
        )

    def final(self):
        # Used by name.ads to `with` the assembly's command-id package
        # (<Assembly>_Commands). self.assembly is set by the base set_assembly,
        # which runs before final(); assembly_name was left None at load() time.
        self.assembly_name = self.assembly.name

        for seq in self.sequences.values():
            for step in seq.steps:
                # Sleep steps don't reference any assembly component or
                # command, so skip assembly-level resolution for them.
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
                # Resolve arg type — static and dynamic are mutually exclusive
                if step.is_dynamic():
                    step.resolve_dynamic_arg_type(step.command_obj, seq)
                    # Auto-populate includes for both the input type and the
                    # leaf arg type so the .adb gets the correct "with" clauses
                    if seq.arg_type_package and seq.arg_type_package not in self.includes:
                        self.includes.append(seq.arg_type_package)
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

    def load_type(self, type_name):
        return model_loader.try_load_model_by_name(type_name, model_types="type")
