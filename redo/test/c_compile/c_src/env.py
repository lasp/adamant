from environments import modify_build_path
import os

this_dir = os.path.dirname(os.path.realpath(__file__))
# Overwrite the normal build path mechanism and just use
# this directory as the build path. This prevents any
# name conflicts we might get from using the regular
# ".all_path".
modify_build_path.set_build_path(this_dir + os.pathsep + os.path.join(this_dir, ".." + os.sep + "ada_src"))
