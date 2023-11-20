import runpy
import os

# load env file in test directory since we will use files in there:
this_dir = os.path.dirname(os.path.realpath(__file__))
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
os.environ["EXTRA_BUILD_PATH"] = this_dir + os.pathsep + os.environ["EXTRA_BUILD_PATH"]
