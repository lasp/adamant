#!/usr/bin/env python3

# Unit test for require_set_parameter_defaults in an assembly model: that an
# assembly with it set loads only when each of its own components sets the
# default value of every one of its parameters, that it does not apply to the
# components of an assembly that includes it, and that a subassembly with it
# set is still checked when another assembly includes it. The models are
# written to a temporary directory and use the test component of the
# parameters component.
import os
import shutil
import sys
import tempfile

import database.setup
from util import model_loader

COMPONENTS = os.path.realpath(os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", "..", "..", "src", "components", "parameters"))

ENV = """from environments import test, modify_build_path  # noqa: F401
modify_build_path.add_to_build_path({path})
"""

ALL_SET = """    set_parameter_defaults:
      - "Parameter_U16 => (Value => 1)"
      - "Parameter_I32 => (Value => 2)"
"""
ONE_SET = """    set_parameter_defaults:
      - "Parameter_U16 => (Value => 1)"
"""


def model(require, component_name, defaults, subassembly=None):
    text = "---\n"
    if require is not None:
        text += "require_set_parameter_defaults: " + str(require) + "\n"
    if subassembly:
        text += "subassemblies:\n  - " + subassembly + "\n"
    return text + "components:\n  - type: Test_Component_1\n    name: " + component_name + "\n" + defaults


MODELS = {
    # A subassembly that requires its parameters to be set, and sets them.
    "set_sub": model(True, "Sub_Component", ALL_SET),
    # An assembly that does not require it, and includes that subassembly.
    "set_parent": model(None, "Parent_Component", "", subassembly="set_sub"),
    # An assembly that requires it and leaves a parameter out.
    "unset_alone": model(True, "Alone_Component", ONE_SET),
    # An assembly that leaves a parameter out and does not require it.
    "unset_allowed": model(False, "Allowed_Component", ONE_SET),
    # A subassembly that requires it and leaves both parameters out.
    "unset_sub": model(True, "Unset_Sub_Component", ""),
    # An assembly that does not require it, and includes that subassembly.
    "unset_parent": model(None, "Unset_Parent_Component", ALL_SET, subassembly="unset_sub"),
}


def check(description, condition):
    if not condition:
        print("FAILED: " + description, file=sys.stderr)
        sys.exit(1)


def load_error(name):
    """Return the error raised by loading an assembly, or None if it loads."""
    try:
        loaded = model_loader.try_load_model_by_name(name, model_types="assembly")
    except BaseException as e:
        return str(e)
    check(name + " is found", loaded is not None)
    return None


def run_cases():
    check("an assembly that sets every parameter loads, and so does one that includes it", load_error("set_parent") is None)

    error = load_error("unset_alone")
    check("an assembly that leaves a parameter out does not load", error is not None)
    check("the error names the component", "Alone_Component" in error)
    check("the error names the parameter left out", "Parameter_I32" in error and "Parameter_U16" not in error)

    check("an assembly that does not require it can leave a parameter out", load_error("unset_allowed") is None)

    error = load_error("unset_parent")
    check("an assembly does not load when a subassembly it includes leaves a parameter out", error is not None)
    check("the error names the component of the subassembly", "Unset_Sub_Component" in error)
    check("the error names both parameters left out", "Parameter_U16" in error and "Parameter_I32" in error)
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    # The models are loaded in a sandbox created from a target in the temporary
    # directory, whose env.py puts the test component on the build path.
    os.environ.pop("ENABLE_PERSISTENT_MODEL_CACHE", None)
    directory = os.path.realpath(tempfile.mkdtemp())
    path = [directory] + [os.path.join(COMPONENTS, d) for d in ["test/test_component_1", "", "types"]]
    with open(os.path.join(directory, "env.py"), "w") as f:
        f.write(ENV.format(path=repr(path)))
    for name, text in MODELS.items():
        with open(os.path.join(directory, name + ".assembly.yaml"), "w") as f:
            f.write(text)
    target = os.path.join(directory, "require_set_parameter_defaults_test")
    session = (target, target, target + ".out")
    environment = database.setup.create_sandbox(*session)[1]
    try:
        run_cases()
    finally:
        database.setup.destroy_sandbox(*session, prev_env=environment)
        shutil.rmtree(directory, ignore_errors=True)
