from environments import test, modify_build_path  # noqa: F401
import os

this_dir = os.path.dirname(os.path.realpath(__file__))
modify_build_path.add_to_build_path(
    [
        this_dir,
        # Nibble_Test_Args references Packed_Nibble, which lives with the test
        # component's other command argument types.
        os.path.realpath(os.path.join(this_dir, ".." + os.sep + ".." + os.sep + "test_component")),
        os.path.realpath(os.path.join(this_dir, ".." + os.sep + ".." + os.sep + "..")),
    ]
)
