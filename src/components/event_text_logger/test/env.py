from environments import test  # noqa: F401
import os.path

this_dir = os.path.dirname(os.path.realpath(__file__))
os.environ["EXTRA_BUILD_PATH"] = this_dir + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "event_producer")) + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "..")) + os.pathsep \
                                 + os.path.realpath(os.path.join(this_dir, "event_assembly"))
