import os.path
from util import redo
from util import debug
from base_classes.build_rule_base import build_rule_base
from database.utility_database import utility_database


class build_path(build_rule_base):
    """
    This build rule runs "redo all" from every directory in the
    build path.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Get the build path. We only want to build things
        # that are actually in the path.
        build_path = None
        with utility_database() as db:
            build_path = db.get_build_path()

        # Build everything in path in one shot. We do this with multiple calls to
        # redo, instead of using one parallel call because each redo "all"
        # might require a different database setup. This is the nature of
        # compiling multiple unit tests, main programs, etc.
        dirs = list(build_path.keys())
        to_build = []
        for dir in dirs:
            all = os.path.join(dir, "all")
            debug.debug_print("building " + all)
            to_build.append(all)
        redo.redo_ifchange(to_build)

    # No need to provide these for "redo recursive"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
