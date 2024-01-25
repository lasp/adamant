import os.path
from os import environ
from base_classes.build_rule_base import build_rule_base
from database.model_cache_database import model_cache_database


# This build rule remove the model cache from the filesystem
# so it will need to be recreated from scratch for the next build.
# This can be useful if an output is not generating correctly, and
# a dependency is missing to the yaml file being modified.
class build_clear_cache(build_rule_base):
    # We override build here for performance. With redo clear_cache there is no
    # need to load the source code and models from then entire project, so
    # we just need to build path to include the directory pointed to by
    # redo_1. So set that in the environment for speed, then call the
    # normal implementation of build.
    def build(self, redo_1, redo_2, redo_3):
        directory = os.path.dirname(os.path.abspath(redo_1))
        environ["BUILD_PATH"] = directory + os.pathsep + directory + os.sep + ".."
        super(build_clear_cache, self).build(redo_1, redo_2, redo_3)

    def _build(self, redo_1, redo_2, redo_3):
        with model_cache_database() as db:
            fname = db.filename
            db.destroy()
            import sys
            sys.stderr.write("Removed " + fname + "\n")

    # No need to provide these for "redo clear_cache"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
