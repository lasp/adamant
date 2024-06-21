import os.path
from util import redo
from util import filesystem
from util import debug
from base_classes.build_rule_base import build_rule_base
from database.utility_database import utility_database


class build_recursive(build_rule_base):
    """
    This build rule recursively runs "redo all" from the specified
    directory and below. It ignores all directories that are not in
    the build path.
    """
    def _build(self, redo_1, redo_2, redo_3):
        import database.setup

        # Figure out build directory location
        directory = os.path.dirname(redo_2)

        # Get the build path. We only want to build things
        # that are actually in the path.
        build_path = None
        with utility_database() as db:
            build_path = db.get_build_path()

        # Find all directories below this directory:
        dirs_to_build = (
            [os.path.join(directory, "all")] if directory in build_path else []
        )
        for root, dirnames, files in filesystem.recurse_through_repo(directory):
            for dir in dirnames:
                full_path = os.path.join(root, dir)
                if full_path in build_path:
                    dirs_to_build.append(os.path.join(full_path, "all"))

        # Build all subdirectories. We do this with multiple calls to
        # redo, instead of using one parallel call because each redo "all"
        # might require a different database setup. This is the nature of
        # compiling multiple unit tests, main programs, etc.
        for dir in dirs_to_build:
            debug.debug_print("building " + dir)
        for dir in dirs_to_build:
            database.setup.reset()
            redo.redo_ifchange(dir)

    # No need to provide these for "redo recursive"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
