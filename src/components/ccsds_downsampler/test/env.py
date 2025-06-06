from environments import test, modify_build_path  # noqa: F401
import os.path

this_dir = os.path.dirname(os.path.realpath(__file__))
modify_build_path.add_to_build_path(
    [
        this_dir,
        os.path.realpath(os.path.join(this_dir, "..")),
        os.path.realpath(os.path.join(this_dir, "test_assembly")),
    ]
)
