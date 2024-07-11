import os.path
from util import shell
from util import redo
import shutil
from base_classes.build_rule_base import build_rule_base
from util import filesystem
import sys


def gnatpp_cmd_prefix():
    return "gnatpp -j0 --no-compact --no-alignment --indentation=3 --indent-continuation=2 \
            --max-line-length=5000 --name-mixed-case --attribute-mixed-case \
            --keyword-lower-case --enum-mixed-case --type-mixed-case --eol=unix \
            --comments-unchanged "


class build_pretty(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Get targets for this directory:
        directory = os.path.abspath(os.path.dirname(redo_1))
        out_dir = os.path.join(directory, "build" + os.sep + "pretty")
        prettified = False

        from os import listdir
        from os.path import isfile, join

        onlyfiles = [
            os.path.join(directory, f)
            for f in listdir(directory)
            if isfile(join(directory, f))
        ]

        # Find all the handwritten ada source code in this directory.
        handwritten_source = [
            target
            for target in onlyfiles
            if target.endswith(".ads") or target.endswith(".adb")
        ]
        if handwritten_source:
            # Info print:
            redo.info_print("Prettifying:\n" + "\n".join(handwritten_source))
            prettified = True

            # Form gnatpp command:
            filesystem.safe_makedir(out_dir)
            gnatpp_cmd = (
                gnatpp_cmd_prefix() + " --output-dir " + + out_dir + " "
                + " ".join(handwritten_source)
            )
            shell.run_command(gnatpp_cmd)

        # Now handle python. First write all source to output:
        handwritten_py = [target for target in onlyfiles if target.endswith(".py")]
        if handwritten_py:
            # Info print:
            redo.info_print("Prettifying:\n" + "\n".join(handwritten_py))
            prettified = True

            filesystem.safe_makedir(out_dir)
            for pyfile in handwritten_py:
                shutil.copyfile(pyfile, os.path.join(out_dir, os.path.basename(pyfile)))

            # Run black on all python files:
            black_cmd = "black " + out_dir + "/*.py"
            shell.run_command(black_cmd)

        if prettified:
            sys.stderr.write(
                "Prettified source code can be found in: " + out_dir + "\n"
            )
        else:
            sys.stderr.write("No source code found in this directory to prettify.\n")
            sys.exit(0)

    # No need to provide these for "redo pretty"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
