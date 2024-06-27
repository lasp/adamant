from util import shell
from util import error
import sys
import os

# This module provides python bindings for the redo
# commandline program.


def __form_call_args(command, args):
    """Form call args."""
    if isinstance(args, str):
        args = [args]
    call = [command] + args
    call_str = " ".join(call)
    return call_str


def __invoke_redo_subprocess(command, args=[]):
    """Private function which invokes a redo-like command."""
    if args:
        # Sometimes the number of arguments to redo can exceed the OS
        # limit. To avoid this, if there is over 1000 arguments we will
        # chunk the redo command up into multiple commands with 1000
        # arguments (or less) each.
        if len(args) > 1000:
            def divide_chunks(things, num):
                """
                Yield successive n-sized
                chunks from l.
                """
                # looping till length l
                for i in range(0, len(things), num):
                    yield things[i:i + num]

            # Execute one chunk at a time:
            for chunk in divide_chunks(args, 1000):
                call_str = __form_call_args(command, chunk)
                shell.run_command(call_str)
        else:
            # Execute in one shot:
            call_str = __form_call_args(command, args)
            shell.run_command(call_str)


def redo(args):
    """Call redo with a list of targets."""
    __invoke_redo_subprocess("redo", args)


def redo_ifchange(args):
    """Call redo-ifchange with a list of targets."""
    __invoke_redo_subprocess("redo-ifchange", args)


def redo_ifcreate(args):
    """Call redo-ifcreate with a list of targets."""
    __invoke_redo_subprocess("redo-ifcreate", args)


def redo_always(args):
    """Call redo-always with a list of targets."""
    __invoke_redo_subprocess("redo-always", args)


def redo_ood(args):
    """
    Call redo-ood with a list of targets. This call returns the
    targets that are out of date as a list.
    """
    if args:
        call_str = __form_call_args("redo-ood", args)
        (
            status,
            stdout,
            stderr,
        ) = shell.try_run_command_capture_output(call_str)
        stdout = stdout.strip()
        stderr = stderr.strip()

        if status != 0:
            import sys

            if stdout:
                sys.stderr.write(stdout + "\n")
            if stderr:
                sys.stderr.write(stderr + "\n")
            error.abort(status)

        # Filter any warnings out of the output. We only want file paths.
        to_ret = [path for path in stderr.split("\n") if path.startswith(os.sep)]
        return to_ret
    return []


def info_print(string):
    """Print an error to the screen."""
    GREEN = "\033[32m"
    NO_COLOR = "\033[0m"
    sys.stderr.write(GREEN + "redo  " + string + "\n" + NO_COLOR)
    sys.stderr.flush()


def info_print_bold(string):
    """Print an error to the screen."""
    GREEN = "\033[32m"
    BOLD = "\033[1m"
    NO_COLOR = "\033[0m"
    sys.stderr.write(GREEN + "redo  " + BOLD + string + "\n" + NO_COLOR)
    sys.stderr.flush()


# Main function to facilitate testing this module
# from the commandline.
if __name__ == "__main__":
    command_dict = {
        "redo": redo,
        "redo-ifchange": redo_ifchange,
        "redo-ifcreate": redo_ifcreate,
        "redo-always": redo_always,
        "redo-ood": redo_ood,
    }

    if len(sys.argv) >= 2:
        command = sys.argv[1]
        args = sys.argv[2:]
        if command in command_dict:
            ret = command_dict[command](args)
            if ret:
                print(str(ret))
            sys.exit(0)

    print("Usage: redo <redo|redo-ifchange|redo-ifcreate|redo-always|redo-ood> args")
    sys.exit(1)
