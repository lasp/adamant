from environments import modify_build_path
import runpy
import os

# load env file in test directory since we will use files in there:
this_dir = os.path.dirname(os.path.realpath(__file__))
runpy.run_path(os.path.join(this_dir, ".." + os.sep + ".." + os.sep + "test" + os.sep + "test_assembly" + os.sep + "env.py"))
modify_build_path.add_to_build_path(this_dir)
