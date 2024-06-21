from database.redo_target_database import redo_target_database
import os.path
import sys
from util import redo
from util import redo_arg
from util import error
from util import shell
from util import filesystem
from base_classes.build_rule_base import build_rule_base
import re


def escape_ansi(line):
    """Used to remove ansi characters from aunit output."""
    ansi_escape = re.compile(r"(?:\x1B[@-_]|[\x80-\x9F])[0-?]*[ -/]*[@-~]")
    return ansi_escape.sub("", line)


class build_test(build_rule_base):
    """
    This build rule runs a compiled test binary. If a binary
    called test.elf is found for a certain directory, then
    this rule will run that test binary. It will also inspect
    the test output. If the output contains the word "FAIL"
    then this rule will return 1 to the commandline. Otherwise
    it will return 0.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Get targets for this directory:
        directory = os.path.abspath(os.path.dirname(redo_1))
        with redo_target_database() as db:
            targets = db.get_targets_for_directory(directory)

        # Look for a test target in the directory:
        binary = None
        for target in targets:
            if redo_arg.in_build_bin_dir(target) and target.endswith("test.elf"):
                binary = target
                break

        if not binary:
            error.error_abort(
                "No target test.elf can be built in this directory '"
                + directory
                + "'. A test.adb must exist."
            )

        # Build files:
        redo.redo_ifchange(binary)

        # Make the log directory:
        log_dir = redo_arg.get_src_dir(binary) + os.sep + "build" + os.sep + "log"
        filesystem.safe_makedir(log_dir)

        # Run the binary and send its output to stderr:
        rc, stderr, stdout = shell.try_run_command_capture_output(binary + " >&2")

        # Write the output to the terminal:
        sys.stderr.write(str(stderr))
        sys.stderr.write(str(stdout))

        # Write the output to a file:
        log_file = log_dir + os.sep + os.path.basename(binary) + ".log"
        with open(log_file, "w") as f:
            f.write(escape_ansi(str(stderr)))
            f.write(escape_ansi(str(stdout)))

        # Abort if the command failed.
        if rc != 0 or (stderr and "FAIL" in stderr) or (stdout and "FAIL" in stdout):
            error.abort()

        # For some reason AUnit does not produce an errored return status if
        # no assertions failed but an unexpected error occurred. Fix this:
        if bool(re.search(r"Unexpected\s+Errors:\s+[1-9][0-9]*", stdout)) or bool(
            re.search(r"Unexpected\s+Errors:\s+[1-9][0-9]*", stderr)
        ):
            error.abort()

    def input_file_regex(self):
        """Match any binary file called "test.elf"."""
        return r".*\/test\.elf$"

    def output_filename(self, input_filename):
        """
        There is no real output name here. This is a dummy name
        that will produce no content.
        """
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(directory, "test")
