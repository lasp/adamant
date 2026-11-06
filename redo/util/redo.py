from util import shell
from util import error
import sys
import os

# This module provides python bindings for the redo
# commandline program.

# Maximum number of arguments per shell invocation of redo. When the argument
# list exceeds this, it is split into chunks to avoid OS limits.
_MAX_SHELL_ARGS = 1000


def __redo_shell_calls(command, args):
    """Yield the command strings that invoke a redo-like command on args
    through the shell, in chunks of _MAX_SHELL_ARGS arguments."""
    if isinstance(args, str):
        args = [args]
    for i in range(0, len(args), _MAX_SHELL_ARGS):
        yield " ".join([command] + args[i:i + _MAX_SHELL_ARGS])


def __argument_budget():
    """
    The number of bytes the arguments of one redo invocation may take.
    Linux limits all arguments and the environment of a program together to
    ARG_MAX, a quarter of the stack size limit, counting each argument's
    bytes, its terminating null, and its pointer. Half of what the
    environment leaves is used, as a margin.
    """
    environment = sum(len(key) + len(value) + 2 + 8 for key, value in os.environ.items())
    return (os.sysconf("SC_ARG_MAX") - environment) // 2


def __argument_size(arg):
    """The bytes one argument takes against ARG_MAX."""
    return len(arg.encode()) + 1 + 8


def __redo_calls(command, args=None, prefix_args=None):
    """Yield the argument lists that invoke a redo-like command on args.

    Each invocation runs directly, without a shell, so every target is its
    own argument. When the arguments exceed what one invocation may take,
    they are split into chunks and one argument list is yielded per chunk.
    If prefix_args is provided, those arguments are prepended to EVERY
    chunk (not just the first). This is critical for commands like
    redo-done where the first argument (the target) must appear in every
    invocation.
    """
    if args is None:
        args = []
    if isinstance(args, str):
        args = [args]
    if prefix_args is None:
        prefix_args = []
    if not prefix_args + args:
        return
    budget = __argument_budget()
    base = [command] + prefix_args
    base_size = sum(__argument_size(arg) for arg in base)
    chunk = []
    chunk_size = base_size
    for arg in args:
        size = __argument_size(arg)
        if chunk and chunk_size + size > budget:
            yield base + chunk
            chunk = []
            chunk_size = base_size
        chunk.append(arg)
        chunk_size += size
    yield base + chunk


def __invoke_redo_subprocess(command, args=None, prefix_args=None):
    """Private function which invokes a redo-like command, in chunks when
    the arguments are too long for one invocation. Output goes to the
    terminal and a failing invocation aborts."""
    for call in __redo_calls(command, args, prefix_args):
        shell.run_program(call)


def __invoke_redo_subprocess_capture_output(command, args=None, prefix_args=None):
    """Private function which invokes a redo-like command, in chunks when
    the arguments are too long for one invocation, capturing its output.
    Returns the concatenated (stdout, stderr) of every invocation. A failing
    invocation prints what it captured and aborts."""
    stdout_all = []
    stderr_all = []
    for call in __redo_calls(command, args, prefix_args):
        status, stdout, stderr = shell.try_run_program_capture_output(call)
        if status != 0:
            if stdout.strip():
                sys.stderr.write(stdout.strip() + "\n")
            if stderr.strip():
                sys.stderr.write(stderr.strip() + "\n")
            error.abort(status)
        stdout_all.append(stdout)
        stderr_all.append(stderr)
    return "\n".join(stdout_all), "\n".join(stderr_all)


def redo(args):
    """
    Call redo with a list of targets. This call runs through the shell, so
    args may end with shell redirections, such as "1>&2" or "2>" + log, to
    direct the output of redo.
    """
    for call_str in __redo_shell_calls("redo", args):
        shell.run_command(call_str)


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
    _, stderr = __invoke_redo_subprocess_capture_output("redo-ood", args)
    # Filter any warnings out of the output. We only want file paths.
    return [path for path in stderr.strip().split("\n") if path.startswith(os.sep)]


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
