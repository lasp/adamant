from util import filesystem
import os.path
from util import redo
from util import error
from util import shell
from util import redo_arg
from util import target
from base_classes.build_rule_base import build_rule_base
import database.model_database
import platform


class build_type_ranges_yaml(build_rule_base):
    """
    This build rule runs a binary file. It matches any binary files
    that are produced with the name "main.elf" and is able to
    execute them.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Find the .elf file that will be used to produce this yaml file.
        directory = redo_arg.get_src_dir(redo_1)
        (
            model_name,
            model_type,
            specific_name,
        ) = database.model_database.split_model_file_name(redo_1)
        the_target = target.try_get_target()
        build_for = platform.system()
        if the_target:
            if the_target.endswith("_Test"):
                build_for += "_Test"
            if the_target.endswith("_Deprecated"):
                build_for += "_Deprecated"
        elf = (
            directory
            + os.sep
            + "build"
            + os.sep
            + "bin"
            + os.sep
            + build_for
            + os.sep
            + model_name
            + "_type_ranges.elf"
        )

        # Build the elf file if it doesn't already exist:
        redo.redo_ifchange(elf)

        # Run the binary:
        rc, stdout, stderr = shell.try_run_command_capture_output(elf)
        if rc != 0:
            error.error_abort("Error running '" + elf + "'.")
            error.error_abort(stderr, code=rc)

        # Print the output:
        filesystem.safe_makedir(os.path.dirname(redo_1))
        print(stdout)

    def input_file_regex(self):
        """Match files with the name "main.elf" """
        return r".*\/.*_type_ranges.elf$"

    def output_filename(self, input_filename):
        """
        The output "file" is not really a file, so it will
        produce no output. But this function gives "redo
        what" the ability to detect when a "redo run" rule
        is available on the system.
        """
        base = redo_arg.get_base_no_ext(input_filename)
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(
            directory,
            "build"
            + os.sep
            + "yaml"
            + os.sep
            + base.replace("_type_ranges", "")
            + ".type_ranges.yaml",
        )
