import os.path
from util import redo
from util import debug
from base_classes.build_rule_base import build_rule_base
import subprocess
import concurrent.futures


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
        skip_dirs = {"alire"}
        for root, dirnames, files in os.walk(directory):
            dirnames[:] = [d for d in dirnames if not d[0] == "." and not d[0] == "_" and d not in skip_dirs]
            if "build" in dirnames:
                dirs_to_clean.append(os.path.join(root, "build"))
                # We don't want to recurse into build directories
                # so remove them from the list.
                dirnames.remove("build")
            if "clean.do" in files and root != directory:
                do_files_to_do.append(os.path.join(root, "clean"))

        # Concurrently remove build directories using system calls. This
        # executes much faster that python's rmtree.
        def remove_directory(dir_to_clean):
            debug.debug_print("removing " + dir_to_clean)
            subprocess.run(["rm", "-rf", dir_to_clean], check=True)

        with concurrent.futures.ThreadPoolExecutor() as executor:
            futures = [executor.submit(remove_directory, d) for d in dirs_to_clean]
            for future in concurrent.futures.as_completed(futures):
                try:
                    future.result()
                except subprocess.CalledProcessError as e:
                    debug.debug_print("Error removing directory: " + str(e))

        # Clean any subdirectories:
        if do_files_to_do:
            redo.redo(do_files_to_do)

    # No need to provide these for "redo clean"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
