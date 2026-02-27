from util import error
import platform
import os

# This module provides useful functions for setting
# and getting the current build target, ie. something
# like "Linux".


def get_target():
    """
    Get the target. If it does not exist, abort with error
    message
    """
    try:
        t = os.environ["TARGET"]
        if t:
            return t
        else:
            error.error_print("Environment variable 'TARGET' not set!")
            error.abort()
    except KeyError:
        error.error_print("Environment variable 'TARGET' not set!")
        error.abort()


def try_get_target():
    """Get the target. If it does not exist, return None"""
    try:
        t = os.environ["TARGET"]
        if t:
            return t
        else:
            return None
    except KeyError:
        return None


def get_default_target():
    """
    Get the target, if it does not exist, return the
    default target which is the result of running
    "uname" from the command line.
    """
    try:
        t = os.environ["TARGET"]
        if t:
            return t
        else:
            return platform.system()
    except KeyError:
        return platform.system()


def set_default_target():
    """
    Set the current target to the default target which
    is the result of running "uname" from the command
    line, ie.
    Linux
    """
    os.environ["TARGET"] = platform.system()


def set_default_test_target():
    """
    Set the target to the default test target which
    is the result of running "uname" from the command
    line and appending it with "_Test", ie.
    Linux_Test
    """
    os.environ["TARGET"] = platform.system() + "_Test"


def set_default_coverage_target():
    """
    Set the target to the default coverage target which
    is the result of running "uname" from the command
    line and appending it with "_Coverage", ie.
    Linux_Coverage
    """
    os.environ["TARGET"] = platform.system() + "_Coverage"


def set_default_prove_target():
    """
    Set the target to the default prove target which
    is the result of running "uname" from the command
    line and appending it with "_Prove", ie.
    Linux_Prove
    """
    os.environ["TARGET"] = platform.system() + "_Prove"


def set_target(target):
    """Set the current target to a given string."""
    os.environ["TARGET"] = target


def set_target_if_not_set(target):
    """Set the current target to a given string."""
    if "TARGET" not in os.environ:
        os.environ["TARGET"] = target


def set_test_target(target):
    """Set the current test target to a given string."""
    if not target.endswith("_Test"):
        target += "_Test"
    os.environ["TARGET"] = target


def unset_target():
    """
    Unset the current target, removing it
    from the environment.
    """
    del os.environ["TARGET"]
