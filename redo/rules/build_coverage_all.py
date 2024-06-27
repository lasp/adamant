import os.path
import sys
from util import redo
from util import error
from util import filesystem
from util import shell
from base_classes.build_rule_base import build_rule_base

# Definitions for producing colored text on the terminal
NO_COLOR = "\033[0m"
BOLD = "\033[1m"
RED = "\033[31m"
GREEN = "\033[32m"
PASSED = BOLD + GREEN + "PASSED" + NO_COLOR
FAILED = BOLD + RED + "FAILED" + NO_COLOR


class build_coverage_all(build_rule_base):
    """
    This build rule looks for unit tests in the directory
    it is passed and recursively below. It then runs all the
    tests in sequence and prints a test report to the terminal
    as the tests are run. Tests are matched by finding either a
    "test.adb" file or a "test.do" file a directory.
    """
    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for coverage_all

    def build(self, redo_1, redo_2, redo_3):
        import database.setup

        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all build directories below this directory:
        tests = []
        for root, dirnames, files in filesystem.recurse_through_repo(directory):
            if "test.adb" in files or "test.adb.do" in files:
                if ".skip_test" in files or ".skip_coverage" in files:
                    sys.stderr.write("Skipping " + root + "\n")
                else:
                    tests.append(root)

        if not tests:
            sys.stderr.write("No tests found in or below '" + directory + "'.\n")
            error.abort(0)

        # Print the test plan:
        num_tests = "%02d" % len(tests)
        sys.stderr.write(
            "Will be running and gathering coverage information on a total of "
            + num_tests
            + " tests:\n"
        )
        for number, test in enumerate(tests):
            rel_test = os.path.relpath(test, directory)
            sys.stderr.write(
                ("%02d" % (number + 1)) + "/" + num_tests + " " + rel_test + "\n"
            )

        # Turn off debug mode. This isn't really compatible with the
        # unit test print out:
        try:
            del os.environ["DEBUG"]
        except BaseException:
            pass

        # Run tests:
        exit_code = 0
        sys.stderr.write("\nTesting...\n")
        for number, test in enumerate(tests):
            rel_test = os.path.relpath(test, directory)
            sys.stderr.write(
                "{0:60}   ".format(
                    (("%02d" % (number + 1)) + "/" + num_tests + " " + rel_test)[:60]
                )
            )
            sys.stderr.flush()
            database.setup.reset()
            try:
                redo.redo([os.path.join(test, "coverage"), "1>&2", "2>/dev/null"])
                sys.stderr.write(" " + PASSED + "\n")
            except Exception:
                exit_code = 1
                sys.stderr.write(" " + FAILED + "\n")

        if exit_code != 0:
            error.abort(exit_code)

        # Run gcovr at this directory.
        command = "gcovr -r " + directory + " >&2"
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
        output_html = coverage_dir + os.sep + "coverage.html"
        command = (
            "gcovr -r "
            + directory
            + " --html --html-details -o "
            + output_html
            + " >&2"
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(command)
        sys.stderr.write(str(stdout))
        sys.stderr.write(str(stderr))

        # Print some info on commandline for user.
        sys.stderr.write("\n")
        sys.stderr.write("Output text file can be found here: " + coverage_file + "\n")
        sys.stderr.write("Output html files can be found in: " + output_html + "\n")

    # No need to provide these for "redo coverage_all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
