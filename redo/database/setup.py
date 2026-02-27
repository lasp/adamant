import os.path
from util import redo_arg
import runpy


def _run_env_file(redo_1, redo_2, redo_3):
    env_file = os.path.join(redo_arg.get_src_dir(redo_2), "env.py")
    if os.path.isfile(env_file):
        runpy.run_path(env_file)


def _do_setup(redo_1, redo_2, redo_3, sandbox=False):
    # Set environment with top level command:
    os.environ["TOP_LEVEL_TARGET"] = redo_1

    from database import _setup

    _setup._setup(redo_1, redo_2, redo_3, sandbox=sandbox)

    os.environ["ADAMANT_SETUP"] = "TRUE"


def setup(redo_1, redo_2, redo_3):
    """
    This function sets up the databases for the build system if
    they have not been setup yet. In this way, this function can be
    called by each instance of "redo" but will only actually
    setup the database during the first call.
    """
    # If there if a local environment, run that
    # first:
    _run_env_file(redo_1, redo_2, redo_3)

    # See if the build system has already been setup.
    try:
        is_setup = os.environ["ADAMANT_SETUP"]
    except KeyError:
        is_setup = False

    # If the build system is not setup, set it
    # up:
    did_setup = False
    if not is_setup:
        _do_setup(redo_1, redo_2, redo_3, sandbox=False)
        did_setup = True

    # Set environment with current command:
    os.environ["CURRENT_BUILD_TARGET"] = redo_1

    return did_setup


def reset():
    """
    This function alerts the build system that the database
    is not currently setup. After calling the function, a
    subsequent call to "setup()" above, will recreate the
    databases.
    """
    try:
        del os.environ["ADAMANT_SETUP"]
    except BaseException:
        pass


def cleanup(redo_1, redo_2, redo_3):
    """
    This function does any necessary build system cleanup.
    It will only do the cleanup if this is the top level
    instantiation of redo.
    """
    if "TOP_LEVEL_TARGET" in os.environ and os.environ["TOP_LEVEL_TARGET"] == redo_1:
        from database import _setup

        _setup._delayed_cleanup(redo_1, redo_2, redo_3)


def immediate_cleanup(redo_1, redo_2, redo_3):
    if "TOP_LEVEL_TARGET" in os.environ and os.environ["TOP_LEVEL_TARGET"] == redo_1:
        from database import _setup

        _setup._cleanup()


def create_sandbox(redo_1, redo_2, redo_3, target_to_set=None):
    """
    This creates a separate instance of the build system, constructed
    off the current target. This can be useful if you need to hardcode
    a certain target during building, regardless of what the root
    target was.
    """
    # Save off the current target and set the new one.
    from util import target

    if target_to_set:
        prev_target = target.try_get_target()
        target.set_target(target_to_set)
    else:
        target.set_default_target()
        prev_target = target.get_target()

    # If there if a local environment, run that
    # first:
    _run_env_file(redo_1, redo_2, redo_3)

    # Run the setup:
    _do_setup(redo_1, redo_2, redo_3, sandbox=True)

    # Set environment with current command:
    os.environ["CURRENT_BUILD_TARGET"] = redo_1

    # Return the previous target to the caller.
    return prev_target


def destroy_sandbox(redo_1, redo_2, redo_3, target_to_restore=None):
    # Cleanup the sandbox and then reset the build system so it can
    # be created from scratch again.
    immediate_cleanup(redo_1, redo_2, redo_3)
    reset()

    # Restore a previous target:
    if target_to_restore:
        from util import target

        target.set_target(target_to_restore)
