from database.redo_target_database import redo_target_database
import os.path
from os import environ
from util import redo
from base_classes.build_rule_base import build_rule_base
import re


# This build rule builds all possible targets located
# in a directory's "build" subdirectory. This usually
# amounts to compiling all objects and generating all
# possible source code within that directory.
# Note that metrics for source and object files are not
# automatically compiled by redo all.
class build_all(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Get targets for this directory:
        directory = os.path.abspath(os.path.dirname(redo_1))
        build_directory = os.path.join(directory, "build")
        targets = []
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except KeyError:
                pass

        # Regex to match metric files:
        metric_reg = re.compile(r".*build/metric/.*\.txt$")
        assertion_obj_reg = re.compile(r".*build/obj/.*\-assertion.o$")
        c_obj_reg = re.compile(r".*build/obj/.*\-c.o$")
        obj_reg = re.compile(r".*build/obj/.*.o$")

        # Find all the objects that can be built in in this build directory, minus any assertion objects since
        # those are not cross platform, and minus .C packages, since those don't compile for every packed type.
        objects = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and target.endswith(".o")
            and not assertion_obj_reg.match(target)
            and not c_obj_reg.match(target)
        ]

        # Pre build the objects using gprbuild to compile them all together. This is a performance enhancement:
        # First figure out which objects need to be built. We don't want to build things that are already up
        # to date
        if not environ.get("DISABLE_PREBUILD"):
            objects_to_prebuild = redo.redo_ood(objects)
            if objects_to_prebuild:
                import rules.build_object as bo

                bo._precompile_objects(objects_to_prebuild)

        # Now build them proper while tracking dependencies. This will just copy the files from the line above
        # to the appropriate location while calling redo to track the dependencies.
        redo.redo_ifchange(objects)

        # Build all non object targets that are in this "build" directory:
        to_build = []
        for target in targets:
            target_path = os.path.dirname(target)
            # Add all files in build to list, except for:
            #   metric files
            #   packed type assertion files
            if (
                target_path.startswith(build_directory)
                and not metric_reg.match(target)
                and not obj_reg.match(target)
            ):  # Don't compile any objects, we will do this below with 'redo objects'
                # and not assertion_obj_reg.match(target):
                to_build.append(target)

        # Build files - we build objects in bulk using the hidden
        # 'redo objects' command. If prebuild is enabled, then this
        # will be faster then compiling the objects one at a time.
        redo.redo_ifchange(to_build)

    # No need to provide these for "redo all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
