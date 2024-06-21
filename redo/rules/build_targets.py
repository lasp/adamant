import os.path
import sys
from base_classes.build_rule_base import build_rule_base
from database.build_target_database import build_target_database
from os import environ


class build_targets(build_rule_base):
    """
    This build rule lists all the known redo build targets that
    exist and can be used.
    """
    # We override build here for performance. With redo targets there is no
    # need to load the source code and models from then entire project,
    # we just need to build path to include the directory pointed to by
    # redo_1. So set that in the environment for speed, then call the
    # normal implementation of build.
    def build(self, redo_1, redo_2, redo_3):
        directory = os.path.dirname(os.path.abspath(redo_1))
        environ["BUILD_PATH"] = directory + os.pathsep + directory + os.sep + ".."
        super(build_targets, self).build(redo_1, redo_2, redo_3)

    def _build(self, redo_1, redo_2, redo_3):
        targets = []
        with build_target_database() as db:
            targets = db.get_build_target_names()
        targets.sort()

        for target in targets:
            instance = None

            # Get instance from DB:
            with build_target_database() as db:
                try:
                    instance, py_file = db.get_build_target_instance(target)
                except TypeError:
                    # class is abstract, skip
                    continue

            # Get info from instance:
            try:
                gpr_file = instance.gpr_project_file()
                path_files = instance.path_files()
                description = instance.description()
            except AttributeError:
                # Target is "deprecated", skip
                continue

            formatted_path_files = ["." + x + "_path" for x in path_files]
            path_file_str = ", ".join(formatted_path_files)

            sys.stderr.write("\n")
            sys.stderr.write(target + "\n")
            sys.stderr.write("Description:   " + description + "\n")
            sys.stderr.write("Project File:  " + gpr_file + "\n")
            sys.stderr.write("Path Files:    " + ".all_path, " + path_file_str + "\n")
            sys.stderr.write("Python Module: " + py_file + "\n")
            sys.stderr.write("Python Class:  " + target + "\n")

    # No need to provide these for "redo targets"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
