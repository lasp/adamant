from database.redo_target_database import redo_target_database
import os.path
import sys
from base_classes.build_rule_base import build_rule_base
from os import environ


# This build rule lists all the known redo targets that
# can be built in a certain directory on stdout. This
# rule is useful for a user trying to determine what type
# of products can be built in a certain directory.
class build_what(build_rule_base):
    # We override build here for performance. With redo what there is no
    # need to load the source code and models from then entire project,
    # we just need to build path to include the directory pointed to by
    # redo_1. So set that in the environment for speed, then call the
    # normal implementation of build.
    def build(self, redo_1, redo_2, redo_3):
        directory = os.path.dirname(os.path.abspath(redo_1))
        environ["BUILD_PATH"] = directory + os.pathsep + directory + os.sep + ".."
        super(build_what, self).build(redo_1, redo_2, redo_3)

    def _build(self, redo_1, redo_2, redo_3):
        # https://stackoverflow.com/questions/1549509/remove-duplicates-in-a-list-while-keeping-its-order-python
        def uniquify_preserve_order(lst):
            return sorted(set(lst), key=lambda x: lst.index(x))

        # Define the special targets that exist everywhere...
        redo_targets = [
            "all",
            # "path",
            # "recursive",
            "clean",
            "clean_all",
            "clear_cache",
            "templates",
            "publish",
            "targets",
            "prove",
            "analyze",
            "style",
            "pretty",
            "test_all",
            "coverage_all",
            "style_all",
        ]
        directory = os.path.dirname(redo_1)
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except BaseException:
                targets = []
        if targets:
            targets.sort()
            for target in targets:
                rel_target = os.path.relpath(target, directory)
                redo_targets.append(rel_target)
        sys.stderr.write(
            "redo " + "\nredo ".join(uniquify_preserve_order(redo_targets)) + "\n"
        )

    # No need to provide these for "redo what"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
