import os
import os.path
import sys
from util import redo
from util import error
from util import filesystem
from base_classes.build_rule_base import build_rule_base
from shutil import copytree

# Definitions for producing colored text on the terminal
NO_COLOR = "\033[0m"
BOLD = "\033[1m"
RED = "\033[31m"
YELLOW = "\033[33m"
GREEN = "\033[32m"
CYAN = "\033[36m"
if "REDO_ANALYZE_ALL_NO_COLOR" in os.environ and os.environ["REDO_ANALYZE_ALL_NO_COLOR"]:
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"
    NONE = "NONE"
    ERROR = "ERROR"
else:
    HIGH = BOLD + RED + "HIGH" + NO_COLOR
    MEDIUM = BOLD + YELLOW + "MEDIUM" + NO_COLOR
    LOW = BOLD + CYAN + "LOW" + NO_COLOR
    NONE = BOLD + GREEN + "NONE" + NO_COLOR
    ERROR = BOLD + RED + "ERROR" + NO_COLOR


class build_analyze_all(build_rule_base):
    """
    This build rule looks for unit tests in the directory
    it is passed and recursively below. It then runs static
    analysis (redo analyze) on all the tests in sequence and
    prints an analysis report to the terminal as the analysis
    is run. Tests are matched by finding either a "test.adb"
    file or a "test.do" file in a directory.
    """
    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for analyze_all

    def build(self, redo_1, redo_2, redo_3):
        import database.setup

        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all build directories below this directory:
        tests = []
        for root, dirnames, files in filesystem.recurse_through_repo(directory):
            if os.sep + "alire" + os.sep in root:
                pass
            elif "test.do" in files or "test.adb" in files or "test.adb.do" in files:
                if ".skip_test" in files:
                    sys.stderr.write("Skipping " + root + "\n")
                elif ".skip_analyze" in files:
                    sys.stderr.write("Skipping " + root + " (analyze disabled)\n")
                else:
                    tests.append(root)

        if not tests:
            sys.stderr.write("No tests found in or below '" + directory + "'.\n")
            error.abort(0)

        # Print the analysis plan:
        num_tests = "%02d" % len(tests)
        sys.stderr.write("Will be running static analysis on a total of " + num_tests + " tests:\n")
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

        # Make a build directory at the top level:
        log_dir = os.path.join(directory, "build" + os.sep + "analysis_logs")
        filesystem.safe_makedir(log_dir)

        # Get the fail level from environment variable
        fail_level = os.environ.get("REDO_ANALYZE_ALL_FAIL_LEVEL", "HIGH").upper()
        if fail_level not in ["HIGH", "MEDIUM", "LOW", "ERROR"]:
            fail_level = "HIGH"  # Default to HIGH if invalid value

        # Track results for exit code determination
        results = []

        # Run static analysis:
        exit_code = 0
        sys.stderr.write("\nAnalyzing...\n")
        for number, test in enumerate(tests):
            rel_test = os.path.relpath(test, directory)
            sys.stderr.write(
                "{0:80}   ".format(
                    (("%02d" % (number + 1)) + "/" + num_tests + " " + rel_test)[:80]
                )
            )
            sys.stderr.flush()
            database.setup.reset()
            try:
                analysis_log = os.path.join(log_dir, rel_test.replace(os.sep, "_") + ".log")
                redo.redo([os.path.join(test, "analyze"), "1>&2", "2>" + analysis_log])

                # Parse the analysis report to determine severity
                report_path = os.path.join(test, "build", "analyze", "report.txt")
                severity = self._parse_analysis_report(report_path)
                results.append(severity)

                if severity == "HIGH":
                    sys.stderr.write(" " + HIGH + "\n")
                elif severity == "MEDIUM":
                    sys.stderr.write(" " + MEDIUM + "\n")
                elif severity == "LOW":
                    sys.stderr.write(" " + LOW + "\n")
                elif severity == "NONE":
                    sys.stderr.write(" " + NONE + "\n")
                else:
                    sys.stderr.write(" " + ERROR + "\n")

            except BaseException:
                results.append("ERROR")
                sys.stderr.write(" " + ERROR + "\n")

            # After analysis, save off the analysis logs for inspection. This is
            # especially useful on a remote CI server.
            try:
                copytree(
                    os.path.join(test, "build" + os.sep + "analyze"),
                    os.path.join(log_dir, rel_test.replace(os.sep, "_"))
                )
            except BaseException:
                pass

        # Determine exit code based on results and fail level
        exit_code = self._determine_exit_code(results, fail_level)
        error.abort(exit_code)

    def _parse_analysis_report(self, report_path):
        """
        Parse the analysis report file to determine the highest severity level.
        Returns "HIGH", "MEDIUM", "LOW", "NONE", or "ERROR" if file read fails.
        """
        if not os.path.exists(report_path):
            return "NONE"

        has_high = False
        has_medium = False
        has_low = False

        try:
            with open(report_path, 'r') as f:
                content = f.read().strip()

            # If file is empty or contains no content, return NONE
            if not content:
                return "NONE"

            # Check for different severity levels in the report
            # Based on the example: "high warning", "medium warning", "medium", "low warning", "low"
            lines = content.split('\n')
            for line in lines:
                line_lower = line.lower().strip()
                if not line_lower:  # Skip empty lines
                    continue
                if 'high warning' in line_lower or 'high:' in line_lower:
                    has_high = True
                elif 'medium warning' in line_lower or 'medium:' in line_lower:
                    has_medium = True
                elif 'low warning' in line_lower or 'low:' in line_lower:
                    has_low = True

        except Exception:
            return "ERROR"

        # Return highest severity found
        if has_high:
            return "HIGH"
        elif has_medium:
            return "MEDIUM"
        elif has_low:
            return "LOW"
        else:
            return "NONE"  # No warnings found

    def _determine_exit_code(self, results, fail_level):
        """
        Determine the exit code based on the analysis results and fail level.

        Args:
            results: List of severity levels from all tests
            fail_level: The threshold level from REDO_ANALYZE_ALL_FAIL_LEVEL

        Returns:
            0 for success, 1 for failure
        """
        # Count occurrences of each severity level
        has_error = "ERROR" in results
        has_high = "HIGH" in results
        has_medium = "MEDIUM" in results
        has_low = "LOW" in results

        # Default behavior: fail on HIGH or ERROR
        if fail_level == "HIGH":
            return 1 if (has_high or has_error) else 0
        elif fail_level == "MEDIUM":
            return 1 if (has_high or has_medium or has_error) else 0
        elif fail_level == "LOW":
            return 1 if (has_high or has_medium or has_low or has_error) else 0
        elif fail_level == "ERROR":
            return 1 if has_error else 0
        else:
            # Default to HIGH behavior if unknown fail level
            return 1 if (has_high or has_error) else 0

    # No need to provide these for "redo analyze_all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
