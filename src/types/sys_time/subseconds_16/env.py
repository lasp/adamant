from environments import modify_build_path
import os

this_dir = os.path.dirname(os.path.realpath(__file__))
modify_build_path.remove_from_build_path(os.path.realpath(os.path.join(this_dir, "..")))
