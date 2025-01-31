from environments import test, modify_build_path  # noqa: F401
import os

this_dir = os.path.dirname(os.path.realpath(__file__))
# Overwrite the normal build path mechanism and just use
# this directory as the build path. This prevents any
# name conflicts we might get from using the regular
# ".all_path".
modify_build_path.add_to_build_path([this_dir, this_dir + os.sep + ".."])
