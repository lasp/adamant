import os.path
import sys
from util import redo
from util import error
from util import filesystem
from util import shell
from base_classes.build_rule_base import build_rule_base
from shutil import copytree

# Definitions for producing colored text on the terminal
NO_COLOR = "\033[0m"
BOLD = "\033[1m"
RED = "\033[31m"
GREEN = "\033[32m"
if "REDO_COVERAGE_ALL_NO_COLOR" in os.environ and os.environ["REDO_COVERAGE_ALL_NO_COLOR"]:
    PASSED = "PASSED"
    FAILED = "FAILED"
else:
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

    def _write_to_both(self, message):
        """Write message to both stderr and summary file."""
        sys.stderr.write(message)
        self.summary_file.write(message)
        self.summary_file.flush()

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

        # Create summary report file
        build_dir = os.path.join(directory, "build")
        filesystem.safe_makedir(build_dir)
        summary_report_path = os.path.join(build_dir, "coverage_all_summary.txt")
        self.summary_file = open(summary_report_path, "w")

        # Print the test plan:
        num_tests = "%02d" % len(tests)
        self._write_to_both(
            "Will be running and gathering coverage information on a total of "
            + num_tests
            + " tests:\n"
        )
        for number, test in enumerate(tests):
            rel_test = os.path.relpath(test, directory)
            self._write_to_both(
                ("%02d" % (number + 1)) + "/" + num_tests + " " + rel_test + "\n"
            )

        # Turn off debug mode. This isn't really compatible with the
        # unit test print out:
        try:
            del os.environ["DEBUG"]
        except BaseException:
            pass

        # Make a build directory at the top level:
        failed_test_log_dir = os.path.join(directory, "build" + os.sep + "failed_coverage_logs")
        log_dir = os.path.join(directory, "build" + os.sep + "coverage_logs")
        filesystem.safe_makedir(failed_test_log_dir)
        filesystem.safe_makedir(log_dir)

        # Run tests:
        exit_code = 0
        self._write_to_both("\nTesting...\n")
        for number, test in enumerate(tests):
            rel_test = os.path.relpath(test, directory)
            self._write_to_both(
                "{0:60}   ".format(
                    (("%02d" % (number + 1)) + "/" + num_tests + " " + rel_test)[:60]
                )
            )
            database.setup.reset()
            try:
                coverage_log = os.path.join(log_dir, rel_test.replace(os.sep, "_") + ".log")
                redo.redo([os.path.join(test, "coverage"), "1>&2", "2>" + coverage_log])
                self._write_to_both(" " + PASSED + "\n")
            except BaseException:
                exit_code = 1
                self._write_to_both(" " + FAILED + "\n")

                # On a failed test, save off the test logs for inspection. This is
                # especially useful on a remote CI server.
                try:
                    copytree(
                        os.path.join(test, "build" + os.sep + "log"),
                        os.path.join(failed_test_log_dir, test.replace(os.sep, "_"))
                    )
                except BaseException:
                    pass

        if exit_code != 0:
            self.summary_file.close()
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

        # Generate multiple filtered HTML reports
        self._write_to_both("\nGenerating filtered coverage reports...\n")

        # 1. Flight code only (exclude test and generated code)
        flight_html = coverage_dir + os.sep + "flight_coverage.html"
        flight_txt = coverage_dir + os.sep + "flight_coverage.txt"
        flight_command = (
            "gcovr -r " + directory +
            " --exclude '.*/test/.*' --exclude '.*/build/.*'" +
            " --html --html-details -o " + flight_html
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(flight_command)
        if rc == 0:
            self._write_to_both("Flight code coverage report generated successfully.\n")
            # Also generate flight code text report
            flight_txt_command = (
                "gcovr -r " + directory +
                " --exclude '.*/test/.*' --exclude '.*/build/.*'"
            )
            rc_txt, stderr_txt, stdout_txt = shell.try_run_command_capture_output(flight_txt_command)
            if rc_txt == 0:
                with open(flight_txt, "w") as f:
                    f.write(stdout_txt)
        else:
            self._write_to_both("Warning: Flight code coverage report generation failed.\n")

        # 2. Test code coverage (test directories only)
        test_html = coverage_dir + os.sep + "test_coverage.html"
        test_command = (
            "gcovr -r " + directory +
            " --filter '.*/test/.*'" +
            " --html --html-details -o " + test_html
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(test_command)
        if rc == 0:
            self._write_to_both("Test code coverage report generated successfully.\n")
        else:
            self._write_to_both("Warning: Test code coverage report generation failed.\n")

        # 3. Generated code coverage (build/src directories)
        generated_html = coverage_dir + os.sep + "generated_coverage.html"
        generated_command = (
            "gcovr -r " + directory +
            " --filter '.*/build/.*'" +
            " --html --html-details -o " + generated_html
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(generated_command)
        if rc == 0:
            self._write_to_both("Generated code coverage report generated successfully.\n")
        else:
            self._write_to_both("Warning: Generated code coverage report generation failed.\n")

        # 4. Complete report - everything above combined
        complete_html = coverage_dir + os.sep + "complete_coverage.html"
        complete_command = (
            "gcovr -r "
            + directory
            + " --html --html-details -o "
            + complete_html
            + " >&2"
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(complete_command)
        sys.stderr.write(str(stdout))
        sys.stderr.write(str(stderr))

        # Print some info on commandline for user.
        self._write_to_both("\n")
        self._write_to_both("Coverage reports generated:\n")
        self._write_to_both("  Flight code only (recommended): " + flight_html + "\n")
        self._write_to_both("  Flight code text summary: " + flight_txt + "\n")
        self._write_to_both("  Test code only: " + test_html + "\n")
        self._write_to_both("  Generated code only: " + generated_html + "\n")
        self._write_to_both("  Complete coverage (all files): " + complete_html + "\n")
        self._write_to_both("  Text summary (all files): " + coverage_file + "\n")

        self.summary_file.close()

    # No need to provide these for "redo coverage_all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
