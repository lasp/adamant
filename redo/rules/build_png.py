import os.path
from util import redo
from util import error
from util import shell
from util import filesystem
from util import redo_arg
from base_classes.build_rule_base import build_rule_base


class build_png(build_rule_base):
    """
    This build rule is capable of turning any ".dot" (graphviz) file
    into a ".png" image, capable of being embedded inside
    a PDF document.
    """
    def _build(self, redo_1, redo_2, redo_3):
        if not redo_arg.in_build_png_dir(redo_1):
            error.error_abort(
                "PNG file '"
                + redo_1
                + "' can only be built in a 'build/png' directory."
            )

        # Depend on the dot file associated with this png. It has to
        # reside in build/dot.
        png_dir, base_name = redo_arg.split_redo_arg(redo_2)
        source_dir = redo_arg.get_src_dir(redo_2)
        dot_dir = os.path.join(source_dir, "build" + os.sep + "dot")
        dot_file = os.path.join(dot_dir, base_name + ".dot")
        redo.redo_ifchange(dot_file)

        # Build the png file:
        filesystem.safe_makedir(png_dir)
        shell.run_command("dot -Tpng " + dot_file + " -o " + redo_3)

    def input_file_regex(self):
        return r".*\.dot$"

    def output_filename(self, input_filename):
        base = redo_arg.get_base_no_ext(input_filename)
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(
            directory, "build" + os.sep + "png" + os.sep + base + ".png"
        )
