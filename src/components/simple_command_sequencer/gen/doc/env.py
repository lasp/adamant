from environments import modify_build_path
import runpy
import os
from util import redo

# The doc depends on these generated files directly, and we don't want to add
# them to the path like we do with the test/test_assembly directory,
# since it will conflict. So we just build them directly instead.
this_dir = os.path.dirname(os.path.realpath(__file__))
_test_assembly_build_dir = os.path.join(
    this_dir, "..", "..", "test", "test_assembly", "build"
)
redo.redo_ifchange(
    [
        os.path.join(_test_assembly_build_dir, "src", basename)
        for basename in [
            "test_assembly_command_sequences_example_sequences.ads",
            "test_assembly_command_sequences_example_sequences.adb",
            "test_assembly_command_sequences_example_sequences_commands.ads",
        ]
    ]
    + [
        os.path.join(
            _test_assembly_build_dir,
            "yaml",
            "test_assembly_command_sequences_example_sequences_summary_record.record.yaml",
        )
    ]
)

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
modify_build_path.add_to_build_path(this_dir)
