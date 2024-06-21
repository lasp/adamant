from base_classes.build_rule_base import build_rule_base
from database.utility_database import utility_database


class build_print_path(build_rule_base):
    """
    This build rule prints every directory in the
    build path.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Get the build path. We only want to build things
        # that are actually in the path.
        build_path = None
        with utility_database() as db:
            build_path = db.get_build_path()

        dirs = list(build_path.keys())
        import sys

        for dir in dirs:
            sys.stderr.write(dir + "\n")

    # No need to provide these for "redo recursive"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
