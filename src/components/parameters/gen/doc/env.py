from environments import modify_build_path
import runpy
import os
from util import redo

# The doc depends on this file directly, and we don't want to add
# it to the path like we do with the test/test_assembly directory,
# since it will conflict. So we just build it directly instead.
this_dir = os.path.dirname(os.path.realpath(__file__))
redo.redo_ifchange(
    os.path.join(
        this_dir,
        ".."
        + os.sep
        + ".."
        + os.sep
        + "test_grouped"
        + os.sep
        + "test_grouped_assembly"
        + os.sep
        + "build"
        + os.sep
        + "src"
        + os.sep
        + "test_grouped_params.ads"
    )
)

# load env file in test directory since we will use files in there:
runpy.run_path(
    os.path.join(
        this_dir,
        ".."
        + os.sep
        + ".."
        + os.sep
        + "test"
        + os.sep
        + "test_assembly"
        + os.sep
        + "env.py",
    )
)
modify_build_path.add_to_build_path(this_dir)
