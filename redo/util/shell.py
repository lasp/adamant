import subprocess
from util import debug as debug_module
from util import error

# This module provides various utilities for executing
# programs via the commandline shell from python.


def try_run_command(command_string, debug=True):
    """
    Try to run a command, and return the resulting
    return code.
    """
    if debug:
        debug_module.debug_print(command_string)
    # close_fds must be "False" otherwise the file descriptors used by redo
    # are closed. This causes lots of terrible things to happen.
    process = subprocess.Popen(command_string, shell=True, close_fds=False)
    process.communicate()
    return process.returncode


def try_run_command_capture_output(command_string, debug=True):
    """
    Try to run a command, and return the resulting
    return code as well as all captured output
    on both stdout and stderr.
    """
    if debug:
        debug_module.debug_print(command_string)
    # close_fds must be "False" otherwise the file descriptors used by redo
    # are closed. This causes lots of terrible things to happen.
    process = subprocess.Popen(
        command_string,
        shell=True,
        close_fds=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        universal_newlines=True,
    )
    stdout, stderr = process.communicate()
    return process.returncode, stdout, stderr


def run_command(command_string, debug=True):
    """
    Run a command. If the return code is not 0,
    abort execution of the program.
    """
    status = try_run_command(command_string, debug)
    if status != 0:
        error.abort(status)


def run_command_suppress_output(command_string, debug=True):
    status, stdout, stderr = try_run_command_capture_output(command_string, debug)
    stdout = stdout.strip()
    stderr = stderr.strip()

    if debug:
        if stdout:
            debug_module.debug_print(stdout)
        if stderr:
            debug_module.debug_print(stderr)
    if status != 0:
        import sys

        if stdout:
            sys.stderr.write(stdout + "\n")
        if stderr:
            sys.stderr.write(stderr + "\n")
        error.abort(status)
