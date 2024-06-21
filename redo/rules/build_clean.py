import os.path
from util import redo
from util import debug
from shutil import rmtree
from base_classes.build_rule_base import build_rule_base


class build_clean(build_rule_base):
    """
    This build rule recursively "cleans" from the specified
    directory and below. Cleaning involves removing any
    "build" directories that are found, as well as running
    any "clean.do" files found.
    """
    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for clean

    def build(self, redo_1, redo_2, redo_3):
        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all build directories below this directory:
        dirs_to_clean = []
        do_files_to_do = []
        alire_dir = os.path.join(os.environ["ADAMANT_DIR"], "alire")
        for root, dirnames, files in os.walk(directory):
            if root.startswith(alire_dir):
                continue
            dirnames[:] = [d for d in dirnames if not d[0] == "." and not d[0] == "_"]
            if "build" in dirnames:
                dirs_to_clean.append(os.path.join(root, "build"))
            if "clean.do" in files and root != directory:
                do_files_to_do.append(os.path.join(root, "clean"))

        # Remove build directories:
        for dir_to_clean in dirs_to_clean:
            try:
                debug.debug_print("removing " + dir_to_clean)
                rmtree(dir_to_clean)
            except BaseException:
                pass

        # Clean any subdirectories:
        if do_files_to_do:
            redo.redo(do_files_to_do)

    # No need to provide these for "redo clean"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
