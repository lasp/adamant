from database.redo_target_database import redo_target_database
import os.path
from util import redo
from base_classes.build_rule_base import build_rule_base
import re


class build_templates(build_rule_base):
    """
    This build rule builds all possible targets located
    in a directory's "build/template" subdirectory.
    """
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

        # Find all the objects that can be built in in this build directory, minus any assertion objects since
        # those are not cross platform.
        template_reg = re.compile(r".*build/template/.*$")
        template_source = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and template_reg.match(target)
        ]

        # Now build them proper while tracking dependencies. This will just copy the files from the line above
        # to the appropriate location while calling redo to track the dependencies.
        redo.redo_ifchange(template_source)

    # No need to provide these for "redo all"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
