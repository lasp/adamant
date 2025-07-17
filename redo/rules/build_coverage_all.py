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
            if os.sep + "alire" + os.sep in root:
                pass
            elif "test.do" in files or "test.adb" in files or "test.adb.do" in files:
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
            " --exclude '.*/test/.*' --exclude '.*/build/src/.*'" +
            " --html --html-details -o " + flight_html
        )
        rc, stderr, stdout = shell.try_run_command_capture_output(flight_command)
        if rc == 0:
            self._write_to_both("Hand code coverage report generated successfully.\n")
            # Also generate hand code text report
            flight_txt_command = (
                "gcovr -r " + directory +
                " --exclude '.*/test/.*' --exclude '.*/build/src/.*'"
            )
            rc_txt, stderr_txt, stdout_txt = shell.try_run_command_capture_output(flight_txt_command)
            if rc_txt == 0:
                with open(flight_txt, "w") as f:
                    f.write(stdout_txt)
        else:
            self._write_to_both("Warning: Hand code coverage report generation failed.\n")

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
            " --filter '.*/build/src/.*'" +
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

        # Generate index.html with links to all reports
        index_html = coverage_dir + os.sep + "index.html"
        self._generate_index_html(index_html, flight_html, flight_txt, test_html, generated_html, complete_html, coverage_file)

        # Print some info on commandline for user.
        self._write_to_both("\n")
        self._write_to_both("Coverage reports generated:\n")
        self._write_to_both("  Index page: " + index_html + "\n")
        self._write_to_both("  Hand code only (recommended): " + flight_html + "\n")
        self._write_to_both("  Hand code text summary: " + flight_txt + "\n")
        self._write_to_both("  Test code only: " + test_html + "\n")
        self._write_to_both("  Generated code only: " + generated_html + "\n")
        self._write_to_both("  Complete coverage (all files): " + complete_html + "\n")
        self._write_to_both("  Text summary (all files): " + coverage_file + "\n")

        self.summary_file.close()

        if exit_code != 0:
            error.abort(exit_code)

    def _generate_index_html(self, index_path, flight_html, flight_txt, test_html, generated_html, complete_html, coverage_txt):
        """Generate a beautiful index.html page with links to all coverage reports."""
        import os
        from datetime import datetime

        # Get just the filenames for relative links
        flight_name = os.path.basename(flight_html)
        test_name = os.path.basename(test_html)
        generated_name = os.path.basename(generated_html)
        complete_name = os.path.basename(complete_html)
        coverage_txt_name = os.path.basename(coverage_txt)

        html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Adamant Unit Test Coverage Report</title>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 800px;
            margin: 0 auto;
            padding: 40px 20px;
            background-color: #f5f7fa;
        }}

        .container {{
            background-color: white;
            padding: 40px;
            border-radius: 12px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}

        h1 {{
            text-align: center;
            color: #2c3e50;
            margin-bottom: 40px;
            font-size: 2.5em;
            font-weight: 300;
            border-bottom: 3px solid #3498db;
            padding-bottom: 20px;
        }}

        .report-section {{
            margin-bottom: 30px;
            padding: 20px;
            border-left: 4px solid #3498db;
            background-color: #f8f9fa;
            border-radius: 0 8px 8px 0;
        }}

        .report-section.recommended {{
            border-left-color: #27ae60;
            background-color: #f0f8f0;
        }}

        .report-title {{
            font-size: 1.3em;
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 10px;
        }}

        .report-title.recommended {{
            color: #27ae60;
        }}

        .report-description {{
            color: #666;
            margin-bottom: 15px;
            font-size: 0.95em;
        }}

        .report-links {{
            display: flex;
            gap: 15px;
            flex-wrap: wrap;
        }}

        .report-link {{
            display: inline-block;
            padding: 10px 20px;
            background-color: #3498db;
            color: white;
            text-decoration: none;
            border-radius: 6px;
            transition: background-color 0.3s ease;
            font-weight: 500;
        }}

        .report-link:hover {{
            background-color: #2980b9;
            text-decoration: none;
        }}

        .report-link.text {{
            background-color: #95a5a6;
        }}

        .report-link.text:hover {{
            background-color: #7f8c8d;
        }}

        .report-link.recommended {{
            background-color: #27ae60;
        }}

        .report-link.recommended:hover {{
            background-color: #219a52;
        }}

        .badge {{
            display: inline-block;
            background-color: transparent;
            color: #27ae60;
            padding: 2px 6px;
            border: 1px solid #27ae60;
            border-radius: 12px;
            font-size: 0.75em;
            font-weight: 500;
            margin-left: 8px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }}

        .footer {{
            text-align: center;
            margin-top: 40px;
            color: #7f8c8d;
            font-size: 0.9em;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Adamant Unit Test Coverage Report</h1>

        <div class="report-section recommended">
            <div class="report-title recommended">
                Hand Code Coverage <span class="badge">RECOMMENDED</span>
            </div>
            <div class="report-description">
                Coverage analysis focused on handwritten source code only. This report excludes test code and auto-generated files.
            </div>
            <div class="report-links">
                <a href="{flight_name}" class="report-link recommended">View HTML Report</a>
            </div>
        </div>

        <div class="report-section">
            <div class="report-title">Complete Code Coverage</div>
            <div class="report-description">
                Comprehensive coverage analysis including all code: handwritten source, tests, and generated code.
            </div>
            <div class="report-links">
                <a href="{complete_name}" class="report-link">View HTML Report</a>
                <a href="{coverage_txt_name}" class="report-link text">View Text Summary</a>
            </div>
        </div>

        <div class="report-section">
            <div class="report-title">Generated Code Coverage</div>
            <div class="report-description">
                Coverage analysis of auto-generated code. This report is useful for
                 verifying that generated code paths are properly exercised by tests.
            </div>
            <div class="report-links">
                <a href="{generated_name}" class="report-link">View HTML Report</a>
            </div>
        </div>

        <div class="report-section">
            <div class="report-title">Test Code Coverage</div>
            <div class="report-description">
                Coverage analysis of test code itself.
            </div>
            <div class="report-links">
                <a href="{test_name}" class="report-link">View HTML Report</a>
            </div>
        </div>

        <div class="footer">
            Generated by <a href="https://github.com/lasp/adamant" target="_blank" style="color: #3498db;
             text-decoration: none;">Adamant</a> on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
        </div>
    </div>
</body>
</html>"""

        with open(index_path, 'w') as f:
            f.write(html_content)

    # No need to provide these for "redo coverage_all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
