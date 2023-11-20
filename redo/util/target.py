from util import error
import platform
import os

# This module provides useful functions for setting
# and getting the current build target, ie. something
# like "Linux".


# Get the target. If it does not exist, abort with error
# message
def get_target():
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


# Get the target. If it does not exist, return None
def try_get_target():
    try:
        t = os.environ["TARGET"]
        if t:
            return t
        else:
            return None
    except KeyError:
        return None


# Get the target, if it doesn not exist, return the
# default target which is the result of running
# "uname" from the command line.
def get_default_target():
    try:
        t = os.environ["TARGET"]
        if t:
            return t
        else:
            return platform.system()
    except KeyError:
        return platform.system()


# Set the current target to the default target which
# is the result of running "uname" from the command
# line, ie.
# Linux
def set_default_target():
    os.environ["TARGET"] = platform.system()


# Set the target to the default test target which
# is the result of running "uname" from the command
# line and appending it with "_Test", ie.
# Linux_Test
def set_default_test_target():
    os.environ["TARGET"] = platform.system() + "_Test"


# Set the target to the default coverage target which
# is the result of running "uname" from the command
# line and appending it with "_Coverage", ie.
# Linux_Coverage
def set_default_coverage_target():
    os.environ["TARGET"] = platform.system() + "_Coverage"


# Set the target to the default prove target which
# is the result of running "uname" from the command
# line and appending it with "_Prove", ie.
# Linux_Prove
def set_default_prove_target():
    os.environ["TARGET"] = platform.system() + "_Prove"


# Set the current target to a given string.
def set_target(target):
    os.environ["TARGET"] = target


# Set the current test target to a given string.
def set_test_target(target):
    if not target.endswith("_Test"):
        target += "_Test"
    os.environ["TARGET"] = target


# Unset the current target, removing it
# from the environment.
def unset_target():
    del os.environ["TARGET"]
