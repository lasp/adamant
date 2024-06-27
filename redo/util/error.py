import sys

# This modules supports the printing of error messages
# and aborting for the system. Error messages are printed
# in red.

# Terminal color definitions:
# YELLOW='\033[33m'
# GREEN='\033[32m'


def error_print(string):
    """Print an error to the screen."""
    RED = "\033[31m"
    BOLD = "\033[1m"
    NO_COLOR = "\033[0m"
    sys.stderr.write(BOLD + RED + string + "\n" + NO_COLOR)
    sys.stderr.flush()


def warning_print(string):
    """Print a warning to the screen."""
    sys.stderr.write(string + "\n")
    sys.stderr.flush()


def abort(code=1):
    """
    Quit execution, and exit with a
    specific error code.
    """
    sys.exit(code)


def error_abort(string, code=1):
    """
    Print an error message and then exit
    with a specific error code.
    """
    error_print(string)
    abort(code)
