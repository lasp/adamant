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
GREEN = "\033[32m"
if "REDO_STYLE_ALL_NO_COLOR" in os.environ and os.environ["REDO_STYLE_ALL_NO_COLOR"]:
    PASSED = "PASSED"
    FAILED = "FAILED"
else:
    PASSED = BOLD + GREEN + "PASSED" + NO_COLOR
    FAILED = BOLD + RED + "FAILED" + NO_COLOR


class build_style_all(build_rule_base):
    """
    This build rule looks for source code in the directory
    it is passed and recursively below. It then runs redo style
    prints a report to the terminal as the style checks are run.
    """

    def _write_to_both(self, message):
        """Write message to both stderr and summary file."""
        sys.stderr.write(message)
        self.summary_file.write(message)
        self.summary_file.flush()

    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for style

    def build(self, redo_1, redo_2, redo_3):
        import database.setup

        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all build directories below this directory:
        tests = []
        for root, dirnames, files in filesystem.recurse_through_repo(directory):
            if os.sep + "alire" + os.sep in root:
                pass
            elif os.sep + "gen" + os.sep + "templates" in root:
                pass
            elif os.sep + "gen" + os.sep + "schemas" in root:
                pass
            else:
                for f in files:
                    if (
                        f.endswith(".ads")
                        or f.endswith(".adb")
                        or f.endswith(".yaml")
                        or f.endswith(".py")
                    ):
                        if ".skip_style" in files:
                            sys.stderr.write("Skipping " + root + "\n")
                        else:
                            tests.append(root)
                        break

        if not tests:
            sys.stderr.write("No source code found in or below '" + directory + "'.\n")
            error.abort(0)

        # Create summary report file
        build_dir = os.path.join(directory, "build")
        filesystem.safe_makedir(build_dir)
        summary_report_path = os.path.join(build_dir, "style_all_summary.txt")
        self.summary_file = open(summary_report_path, "w")

        # Print the test plan:
        num_tests = "%02d" % len(tests)
        self._write_to_both("Will be checking a total of " + num_tests + " directories:\n")
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
        failed_style_log_dir = os.path.join(directory, "build" + os.sep + "failed_style_logs")
        log_dir = os.path.join(directory, "build" + os.sep + "style_logs")
        filesystem.safe_makedir(failed_style_log_dir)
        filesystem.safe_makedir(log_dir)

        # Run tests:
        exit_code = 0
        self._write_to_both("\nChecking style...\n")
        for number, test in enumerate(tests):
            rel_test = os.path.relpath(test, directory)
            self._write_to_both(
                "{0:80}   ".format(
                    (("%02d" % (number + 1)) + "/" + num_tests + " " + rel_test)[:80]
                )
            )
            database.setup.reset()
            try:
                style_run_log = os.path.join(log_dir, rel_test.replace(os.sep, "_") + ".log")
                redo.redo([os.path.join(test, "style"), "1>&2", "2>" + style_run_log])

                # See if there is anything in the log file:
                style_log = os.path.join(
                    os.path.join(os.path.join(test, "build"), "style"), "style.log"
                )
                if os.path.isfile(style_log) and os.path.getsize(style_log) > 0:
                    self._write_to_both(" " + FAILED + "\n")
                    exit_code = 1

                    # On a failed style, save off the logs for inspection. This is
                    # especially useful on a remote CI server.
                    try:
                        copytree(
                            os.path.join(os.path.join(test, "build"), "style"),
                            os.path.join(failed_style_log_dir, test.replace(os.sep, "_"))
                        )
                    except BaseException:
                        pass
                else:
                    self._write_to_both(" " + PASSED + "\n")
            except Exception:
                self._write_to_both(" " + FAILED + "\n")
                exit_code = 1

                # On a failed style, save off the logs for inspection. This is
                # especially useful on a remote CI server.
                try:
                    copytree(
                        os.path.join(os.path.join(test, "build"), "style"),
                        os.path.join(failed_style_log_dir, test.replace(os.sep, "_") + "_style")
                    )
                except BaseException:
                    pass

        self.summary_file.close()
        error.abort(exit_code)

    # No need to provide these for "redo style_all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
