import sys
from os import environ

# This module supports debug printing. When the DEBUG
# environment variable is set, these functions will
# produce text on the screen to stderr (so that they
# do not conflict with redo on stdout).


def is_debug_on():
    return bool(environ.get("DEBUG"))


# Print a debug string to stdout.
def debug_print(string):
    if environ.get("DEBUG"):
        sys.stderr.write(string + "\n")
        sys.stderr.flush()


# Compute a debug string, via func,
# if the DEBUG variable is set. This
# function has the advantage that func
# is not computed if DEBUG is not set.
# It may be more performant to use this
# function instead of "debug_print()"
# if constructing the debug message
# takes a significant amount of time.
def debug_do_print(func):
    if environ.get("DEBUG"):
        sys.stderr.write(func() + "\n")
        sys.stderr.flush()
