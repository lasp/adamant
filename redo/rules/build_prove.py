from database.redo_target_database import redo_target_database
import os.path
import sys
from base_classes.build_rule_base import build_rule_base
from os import environ
from database.source_database import source_database
from util import ada
from rules import build_object
from util import filesystem
from util import redo
from util import shell
import re
from models import prove


def _get_source_files(object_files):
    to_return = []

    def _get_src_for_object(object_file):
        # Get the source files associated with this object:
        # First, let's assume this object can be built from Ada
        # source code.
        package_name = ada.file_name_to_package_name(os.path.basename(object_file))
        source_files = None
        with source_database() as db:
            source_files = db.try_get_sources(package_name)

        return source_files

    for object_file in object_files:
        source_files = _get_src_for_object(object_file)
        if source_files:
            to_return.extend(source_files)

    return to_return


def _prove_ada_sources(source_files, base_dir):
    # Extract useful path info:
    build_dir = base_dir + os.sep + "build" + os.sep + "prove"

    # Make the build directory:
    filesystem.safe_makedir(build_dir)

    # The build target must be Linux_Prove
    build_target = "Linux_Prove"
    build_target_instance, build_target_file = build_object._get_build_target_instance(
        build_target
    )

    # Depend and build the source file:
    deps = source_files + [build_target_file, __file__]

    # Build dependencies:
    redo.redo_ifchange(deps)

    # We cannot use "fast" compilation when canning GNATprove, since GNATprove wants to analyze both
    # the .adb's and .ads's, which is not always true of pure compilation. This will make sure we build
    # and depend on all related source code, not just the minimum set required for compilation.
    environ["SAFE_COMPILE"] = "True"

    # Build all dependencies for these source files (recursively):
    with source_database() as db:
        deps += build_object._build_all_ada_dependencies(
            source_files, db
        )

    # Form the gnatprove command:
    gpr_project_file = build_target_instance.gpr_project_file().strip()
    dep_dirs = list(set([os.path.dirname(f) for f in deps]))
    output_file = build_dir + os.sep + "prove.txt"
    direct = " >&2"

    # Info print:
    redo.info_print("Analyzing:\n" + "\n".join(source_files))

    # See if there is a GNATprove yaml configuration file that determines the mode
    # and level we use for GNATprove.
    mode = "gold"
    level = 2
    prove_yaml = os.path.join(base_dir, "all.prove.yaml")
    if os.path.exists(prove_yaml):
        sys.stderr.write("Using GNATprove configuration found in " + prove_yaml + ".\n")
        prove_model = prove.prove(prove_yaml)
        if prove_model.mode:
            mode = prove_model.mode
        if prove_model.level:
            level = prove_model.level
    else:
        sys.stderr.write(
            "No GNATprove configuration was found at "
            + prove_yaml
            + ". Using default configuration.\n"
        )
    sys.stderr.write(
        "Running GNATprove with switches: --level="
        + str(level)
        + " --mode="
        + mode
        + "\n"
    )

    prove_cmd = (
        "gnatprove -j0 --checks-as-errors --level="
        + str(level)
        + " --mode="
        + mode
        + " -aP "
        + os.environ["ADAMANT_DIR"]
        + os.sep
        + "redo"
        + os.sep
        + "targets"
        + os.sep
        + "gpr"
        + " -P "
        + gpr_project_file
        + " -XADAMANT_DIR="
        + os.environ["ADAMANT_DIR"]
        + " -XOBJECT_DIR="
        + build_dir
        + " -XSOURCE_DIRS="
        + ",".join(dep_dirs)
        + " "
        + " ".join(source_files)
        + direct
    )

    # Run the compile command:
    ret, stdout, stderr = shell.try_run_command_capture_output(prove_cmd)

    # Write output to file:
    with open(build_dir + os.sep + "prove.txt", "w") as f:
        f.write(stdout)
        f.write(stderr)

    # Write output to terminal:
    sys.stderr.write(stdout)
    sys.stderr.write(stderr)
    sys.stderr.write("GNATprove command output saved in " + output_file + "\n")

    return ret


# This build rule uses gnatprove to analyze any SPARK code
# found in the current directory.
class build_prove(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Define the special targets that exist everywhere...
        directory = os.path.abspath(os.path.dirname(redo_1))
        build_directory = os.path.join(directory, "build")
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except BaseException:
                targets = []
        # Find all the objects that can be built in in this build directory, minus any assertion objects since
        # those are not cross platform, and minus .C packages, since those don't compile for every packed type.
        assertion_obj_reg = re.compile(r".*build/obj/.*\-assertion.o$")
        c_obj_reg = re.compile(r".*build/obj/.*\-c.o$")
        objects = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and target.endswith(".o")
            and not assertion_obj_reg.match(target)
            and not c_obj_reg.match(target)
        ]
        if objects:
            sources = _get_source_files(objects)
            ret = _prove_ada_sources(sources, directory)
            # Exit with error code if gnatprove failed:
            if ret != 0:
                sys.exit(ret)
        else:
            sys.stderr.write("No source files found to analyze.\n")

    # No need to provide these for "redo what"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
