import os.path
from util import redo
from base_classes.build_rule_base import build_rule_base
from util import error
from util import filesystem


class build_clean_all(build_rule_base):
    """
    This build rule recursively "cleans" from the top of the
    git repository that the command is run in. Cleaning involves
    removing any "build" directories that are found, as well as
    running any "clean.do" files found.
    """
    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for clean

    def build(self, redo_1, redo_2, redo_3):
        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))
        git_root = filesystem.get_git_root(directory)
        if git_root is None:
            error.error_abort(
                "No valid git repository was found containing the directory: "
                + str(directory)
            )

        # Run redo clean from git root.
        redo.redo(os.path.join(git_root, "clean"))

    # No need to provide these for "redo clean"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
