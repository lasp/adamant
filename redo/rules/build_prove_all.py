import os.path
import re
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
if "REDO_PROVE_ALL_NO_COLOR" in os.environ and os.environ["REDO_PROVE_ALL_NO_COLOR"]:
    PASSED = "PASSED"
    FAILED = "FAILED"
else:
    PASSED = BOLD + GREEN + "PASSED" + NO_COLOR
    FAILED = BOLD + RED + "FAILED" + NO_COLOR

SPARK_ON_REGEX = re.compile(r"SPARK_Mode\s*=>\s*On")


def _dir_has_spark_on(files, root):
    """Return True if any Ada source in this directory enables SPARK."""
    for f in files:
        if f.endswith(".ads") or f.endswith(".adb"):
            try:
                with open(os.path.join(root, f), "r", errors="ignore") as fh:
                    if SPARK_ON_REGEX.search(fh.read()):
                        return True
            except OSError:
                pass
    return False


class build_prove_all(build_rule_base):
    """
    This build rule looks for SPARK packages in the directory it is
    passed and recursively below. A directory is eligible if it contains
    an "all.prove.yaml" GNATprove configuration or any source with
    "SPARK_Mode => On". It then runs "redo prove" in each eligible
    directory in sequence and prints a report to the terminal as the
    proofs are run. Eligibility is recomputed from the source tree on
    every run, so a package converted to SPARK is picked up
    automatically. Note that "redo prove" analyzes a directory's full
    object closure, including generated code, so SPARK inside autocode
    is proved through its owning directory.

    A directory containing a ".skip_prove" marker is skipped along with
    everything below it. This is intended for deliberate proof-failure
    fixtures used to test the build system itself.
    """

    def _write_to_both(self, message):
        """Write message to both stderr and summary file."""
        sys.stderr.write(message)
        self.summary_file.write(message)
        self.summary_file.flush()

    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for prove_all

    def build(self, redo_1, redo_2, redo_3):
        import database.setup

        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all SPARK directories below this directory:
        prove_dirs = []
        for root, dirnames, files in filesystem.recurse_through_repo(directory):
            if os.sep + "gen" + os.sep + "templates" in root:
                pass  # template text, not compilable source
            elif ".skip_prove" in files:
                sys.stderr.write("Skipping " + root + "\n")
                dirnames[:] = []  # also skip everything below
            elif "all.prove.yaml" in files or _dir_has_spark_on(files, root):
                prove_dirs.append(root)
        prove_dirs.sort()

        if not prove_dirs:
            sys.stderr.write(
                "No SPARK packages found in or below '" + directory + "'.\n"
            )
            error.abort(0)

        # Create summary report file
        build_dir = os.path.join(directory, "build")
        filesystem.safe_makedir(build_dir)
        summary_report_path = os.path.join(build_dir, "prove_all_summary.txt")
        self.summary_file = open(summary_report_path, "w")

        # Print the prove plan:
        num_dirs = "%02d" % len(prove_dirs)
        self._write_to_both(
            "Will be proving a total of " + num_dirs + " SPARK packages:\n"
        )
        for number, prove_dir in enumerate(prove_dirs):
            rel_dir = os.path.relpath(prove_dir, directory)
            self._write_to_both(
                ("%02d" % (number + 1)) + "/" + num_dirs + " " + rel_dir + "\n"
            )

        # Turn off debug mode. This isn't really compatible with the
        # report print out:
        try:
            del os.environ["DEBUG"]
        except BaseException:
            pass

        # Make a build directory at the top level:
        failed_prove_log_dir = os.path.join(directory, "build" + os.sep + "failed_prove_logs")
        log_dir = os.path.join(directory, "build" + os.sep + "prove_logs")
        filesystem.safe_makedir(failed_prove_log_dir)
        filesystem.safe_makedir(log_dir)

        # Run proofs:
        exit_code = 0
        self._write_to_both("\nProving...\n")
        for number, prove_dir in enumerate(prove_dirs):
            rel_dir = os.path.relpath(prove_dir, directory)
            self._write_to_both(
                "{0:80}   ".format(
                    (("%02d" % (number + 1)) + "/" + num_dirs + " " + rel_dir)[:80]
                )
            )
            database.setup.reset()
            try:
                prove_log = os.path.join(log_dir, rel_dir.replace(os.sep, "_") + ".log")
                redo.redo([os.path.join(prove_dir, "prove"), "1>&2", "2>" + prove_log])
                self._write_to_both(" " + PASSED + "\n")
            except BaseException:
                exit_code = 1
                self._write_to_both(" " + FAILED + "\n")

                # On a failed proof, save off the prove logs for inspection. This is
                # especially useful on a remote CI server.
                try:
                    copytree(
                        os.path.join(prove_dir, "build" + os.sep + "prove"),
                        os.path.join(failed_prove_log_dir, prove_dir.replace(os.sep, "_"))
                    )
                except BaseException:
                    pass

        self.summary_file.close()
        error.abort(exit_code)

    # No need to provide these for "redo prove_all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
