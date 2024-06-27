from database.redo_target_database import redo_target_database
import os.path
import sys
from util import redo
from util import redo_arg
from util import error
from util import shell
from util import filesystem
from util import target as tgt
from base_classes.build_rule_base import build_rule_base


class build_coverage(build_rule_base):
    """
    This build rule runs a compiled test binary. If a binary
    called test.elf is found for a certain directory, then
    this rule will run that test binary. It will also inspect
    the test output. If the output contains the word "FAIL"
    then this rule will return 1 to the commandline. Otherwise
    it will return 0.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # The coverage build rule requires the default coverage target to be set.
        tgt.set_default_coverage_target()
        build_target = tgt.get_target()

        # Get targets for this directory:
        directory = os.path.abspath(os.path.dirname(redo_1))
        with redo_target_database() as db:
            targets = db.get_targets_for_directory(directory)

        # Look for a coverage target in the directory:
        binary = None
        for target in targets:
            if redo_arg.in_build_bin_dir(target) and (
                target.lower().endswith("coverage" + os.sep + "test.elf")
                or target.lower().endswith("cov" + os.sep + "test.elf")
            ):
                binary = target.strip()
                break

        if not binary:
            error.error_abort(
                "No target test.elf can be built in this directory '"
                + directory
                + "'. A test.adb must exist."
            )

        # Build files:
        binary_src_dir = redo_arg.get_src_dir(binary)
        # Pipe through true, so we can get a coverage report even for a failing test
        redo.redo_ifchange(binary)
        redo.redo(binary_src_dir + os.sep + "test | true")

        # Running the binary will create a bunch of .gcda files which are the files that
        # gcov needs to do analysis. Unfortunately, because of the way redo works, writing
        # outputs to temp files before copying them to the final destination, all of these
        # .gcda files are created in the temporary directories. This is brute force, but should
        # work, let's go through the entire build path, look for these .gcda files and move them
        # to the appropriate object directory.
        from database import _setup
        from glob import glob

        # Find .gcda files in path that are in temp directories:
        build_path = _setup._get_path_from_env("COMPUTED_BUILD_PATH")
        gcda_files = []
        for path in build_path:
            obj_dir = path + os.sep + "build" + os.sep + "obj" + os.sep + build_target
            if os.path.isdir(obj_dir):
                gcda_files.extend(glob(obj_dir + os.sep + "*" + os.sep + "*.gcda"))

        # Move these .gcda file up a directory to the object directory:
        for gcda_file in gcda_files:
            # Move file:
            new_dir = os.path.dirname(os.path.dirname(gcda_file))
            base_name = os.path.basename(gcda_file)
            os.replace(gcda_file, new_dir + os.sep + base_name)

        # Run gcovr on the directory above this test directory.
        src_dir = os.path.dirname(redo_arg.get_src_dir(redo_1))
        command = "gcovr -r " + src_dir + " >&2"
        rc, stderr, stdout = shell.try_run_command_capture_output(command)
        sys.stderr.write(str(stdout))
        sys.stderr.write(str(stderr))

        # Write the output report to a text file:
        coverage_dir = directory + os.sep + "build" + os.sep + "coverage"
        coverage_file = coverage_dir + os.sep + "coverage.txt"
        filesystem.safe_makedir(coverage_dir)
        with open(coverage_file, "w") as f:
            f.write(stdout)

        # Generate html report:
        output_html = coverage_dir + os.sep + "test.html"
        command = (
            "gcovr -r " + src_dir + " --html --html-details -o " + output_html + " >&2"
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(command)
        sys.stderr.write(str(stdout))
        sys.stderr.write(str(stderr))

        # Print some info on commandline for user.
        sys.stderr.write("\n")
        sys.stderr.write("Output text file can be found here: " + coverage_file + "\n")
        sys.stderr.write("Output html files can be found in: " + output_html + "\n")

    def input_file_regex(self):
        """Match any binary file called "test.elf"."""
        return r".*\/test\.elf$"

    def output_filename(self, input_filename):
        """
        There is no real output name here. This is a dummy name
        that will produce no content.
        """
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(directory, "coverage")
