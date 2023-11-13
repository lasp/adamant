from environments import test  # noqa: F401
import os.path

this_dir = os.path.dirname(os.path.realpath(__file__))
os.environ["EXTRA_BUILD_PATH"] = this_dir + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "test_component_1")) + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "test_component_2")) + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "..")) + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "test_assembly"))
