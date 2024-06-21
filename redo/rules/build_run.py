from database.redo_target_database import redo_target_database
import os.path
from util import redo
from util import error
from util import shell
from util import redo_arg
from base_classes.build_rule_base import build_rule_base


class build_run(build_rule_base):
    """
    This build rule runs a binary file. It matches any binary files
    that are produced with the name "main.elf" and is able to
    execute them.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Get targets for this directory:
        directory = os.path.dirname(redo_1)
        with redo_target_database() as db:
            targets = db.get_targets_for_directory(directory)

        # Look for a run target in the directory:
        binary = None
        for target in targets:
            if redo_arg.in_build_bin_dir(target) and target.endswith("main.elf"):
                binary = target
                break

        if not binary:
            error.error_abort(
                "No target main.elf can be built in this directory '"
                + directory
                + "'. A main.adb must exist."
            )

        # Build files:
        redo.redo_ifchange(binary)

        # Run the binary and send its output to stderr:
        shell.run_command(binary + " >&2")

    def input_file_regex(self):
        """Match files with the name "main.elf" """
        return r".*\/main\.elf$"

    def output_filename(self, input_filename):
        """
        The output "file" is not really a file, so it will
        produce no output. But this function gives "redo
        what" the ability to detect when a "redo run" rule
        is available on the system.
        """
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(directory, "run")
