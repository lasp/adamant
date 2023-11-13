import sys

# This modules supports the printing of error messages
# and aborting for the system. Error messages are printed
# in red.

# Terminal color definitions:
# YELLOW='\033[33m'
# GREEN='\033[32m'


# Print an error to the screen.
def error_print(string):
    RED = "\033[31m"
    BOLD = "\033[1m"
    NO_COLOR = "\033[0m"
    sys.stderr.write(BOLD + RED + string + "\n" + NO_COLOR)
    sys.stderr.flush()


# Print a warning to the screen:
def warning_print(string):
    sys.stderr.write(string + "\n")
    sys.stderr.flush()


# Quit execution, and exit with a
# specific error code.
def abort(code=1):
    sys.exit(code)


# Print an error message and then exit
# with a specific error code.
def error_abort(string, code=1):
    error_print(string)
    abort(code)
