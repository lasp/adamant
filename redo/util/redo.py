from util import shell
from util import error
import sys
import os

# This module provides python bindings for the redo
# commandline program.

# Maximum number of arguments per subprocess invocation. When the argument
# list exceeds this, it is split into chunks to avoid OS limits.
_MAX_ARGS = 1000


def __form_call_args(command, args):
    """Form call args."""
    if isinstance(args, str):
        args = [args]
    call = [command] + args
    call_str = " ".join(call)
    return call_str


def __divide_chunks(things, num):
    """Yield successive num-sized chunks from a list."""
    for i in range(0, len(things), num):
        yield things[i:i + num]


def __invoke_redo_subprocess(command, args=None, prefix_args=None):
    """Private function which invokes a redo-like command.

    When the argument list exceeds _MAX_ARGS, it is split into chunks and
    each chunk is invoked separately. If prefix_args is provided, those
    arguments are prepended to EVERY chunk (not just the first). This is
    critical for commands like redo-done where the first argument (the
    target) must appear in every invocation.
    """
    if args is None:
        args = []
    if isinstance(args, str):
        args = [args]
    if prefix_args is None:
        prefix_args = []
    all_args = prefix_args + args
    if not all_args:
        return
    if len(all_args) > _MAX_ARGS:
        # Chunk only the args, not the prefix. Each invocation becomes:
        #   command <prefix_args> <chunk_of_args>
        # This ensures prefix_args (e.g. redo-done's target) appear in
        # every invocation, not just the first.
        chunk_size = max(1, _MAX_ARGS - len(prefix_args))
        for chunk in __divide_chunks(args, chunk_size):
            call_str = __form_call_args(command, prefix_args + chunk)
            shell.run_command(call_str)
    else:
        call_str = __form_call_args(command, all_args)
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


def redo_done(target, deps=None):
    """Call redo-done to register a pre-built target with its dependencies.

    The target is passed as a prefix_arg so it appears as the first argument
    in every chunk. Without this, chunking would treat a dep as the target
    in the second chunk, corrupting its redo database.
    """
    if deps is None:
        deps = []
    __invoke_redo_subprocess("redo-done", deps, prefix_args=[target])


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
