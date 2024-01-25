from database.redo_target_database import redo_target_database
import os.path
import sys
from base_classes.build_rule_base import build_rule_base
from rules import build_object
from util import redo_arg
from util import redo
from util import shell
import re


def _get_target(object_files):
    # Make sure that the build target is the same for all object files. This should be enforced by the
    # build system itself.
    build_target = redo_arg.get_target(object_files[0])
    for obj_file in object_files:
        assert build_target == redo_arg.get_target(
            obj_file
        ), "All build object file must have same build target!"
    return build_target


def _start_server(base_dir, build_target):
    # To support running CodePeer with "-j0" flag we cannot use the shared directory
    # to store the CodePeer output directory (which contains the CodePeer database.
    # So we make sure CodePeer always outputs to a native directory located in
    # ~/.codepeer/absolute/path/to/redo/analysis/dir. This keeps things fast
    # and allows us to use "-j0".
    from pathlib import Path

    home = str(Path.home())
    output_dir = home + os.sep + ".codepeer" + base_dir
    src_dir = output_dir + os.sep + "src"

    # Get the build target instance:
    build_target_instance, build_target_file = build_object._get_build_target_instance(
        build_target
    )

    # Get info for forming codepeer command:
    gpr_project_file = build_target_instance.gpr_project_file().strip()
    direct = " >&2"

    # Info print:
    port = 8081
    redo.info_print("Starting CodePeer IDE Server...")
    sys.stderr.write("Using build target: " + build_target + "\n")
    sys.stderr.write("Using analysis directory: " + output_dir + "\n")
    sys.stderr.write("Connect GNAT Studio on port: " + str(port) + "\n")
    sys.stderr.write(
        ("For more information see "
         "https://docs.adacore.com/codepeer-docs/users_guide/"
         "_build/html/viewing_output.html#accessing-results-remotely-ide-server\n")
    )

    # Form codepeer command:
    gpr_search_path = (
        "export GPR_PROJECT_PATH="
        + os.environ["ADAMANT_DIR"]
        + os.sep
        + "redo"
        + os.sep
        + "targets"
        + os.sep
        + "gpr"
        + ":$GPR_PROJECT_PATH;"
    )
    analyze_cmd = (
        gpr_search_path
        + " codepeer -P "
        + gpr_project_file
        + " -output-dir "
        + output_dir
        + " --ide-server --port="
        + str(port)
        + " -XADAMANT_DIR="
        + os.environ["ADAMANT_DIR"]
        + " -XOBJECT_DIR="
        + output_dir
        + " -XSOURCE_DIRS="
        + src_dir
        + direct
    )

    # Run the compile command:
    shell.run_command(analyze_cmd)


# This build rule uses codepeer to analyze any code
# found in the current directory.
class build_codepeer_server(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Define the special targets that exist everywhere...
        directory = os.path.abspath(os.path.dirname(redo_1))
        build_directory = os.path.join(directory, "build")
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except BaseException:
                targets = []
        # Find all the objects that can be built in in this build directory, minus
        # any assertion and representation objects since those are not flight packages.
        assertion_obj_reg = re.compile(r".*build/obj/.*\-assertion.o$")
        representation_obj_reg = re.compile(r".*build/obj/.*\-representation.o$")
        objects = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and target.endswith(".o")
            and not assertion_obj_reg.match(target)
            and not representation_obj_reg.match(target)
        ]
        if objects:
            target = _get_target(objects)
            ret = _start_server(directory, target)
            # Exit with error code if codepeer failed:
            if ret != 0:
                sys.exit(ret)
        else:
            sys.stderr.write("No source files found to analyze.\n")

    # No need to provide these for "redo codepeer_server"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
