from database.redo_target_database import redo_target_database
import os.path
import sys
from base_classes.build_rule_base import build_rule_base
from os import environ
from database.source_database import source_database
from util import ada
from rules import build_object
from util import redo_arg
from util import filesystem
from util import redo
from util import shell
from models import analyze
import re


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

    # Make sure that the build target is the same for all object files. This should be enforced by the
    # build system itself.
    build_target = redo_arg.get_target(object_files[0])
    for obj_file in object_files:
        assert build_target == redo_arg.get_target(
            obj_file
        ), "All build object file must have same build target!"

    to_return = list(set(to_return))
    return to_return, build_target


def _analyze_ada_sources(source_files, base_dir, build_target, binary_mode=False):
    # Extract useful path info:
    build_dir = base_dir + os.sep + "build" + os.sep + "analyze"
    sources_file = build_dir + os.sep + "sources_analyzed.txt"

    # Make the build directory:
    filesystem.safe_makedir(build_dir)

    # Write all sources to analyze to file in build directory:
    with open(sources_file, "w") as f:
        f.write("\n".join(source_files))

    # Get the build target instance:
    build_target_instance, build_target_file = build_object._get_build_target_instance(
        build_target
    )

    # Depend and build the source file:
    deps = source_files + [build_target_file, __file__]

    # Build dependencies:
    redo.redo_ifchange(deps)

    # We cannot use "fast" compilation when canning CodePeer, since CodePeer wants to analyze both
    # the .adb's and .ads's, which is not always true of pure compilation. This will make sure we build
    # and depend on all related source code, not just the minimum set required for compilation.
    environ["SAFE_COMPILE"] = "True"

    # Build all dependencies for these source files (recursively):
    with source_database() as db:
        deps += build_object._build_all_ada_dependencies(
            source_files, db
        )
    deps = list(set(deps))

    # We behave a bit differently when analyzing a directory that can produce a binary (.elf)
    # vs. a directory that can only produce objects. In the latter case, we only analyze
    # the source code found in this directory. In the former case, we analyze ALL the source
    # used to create the binary.
    if binary_mode:
        analyzing_what = "Binary"
        sources_to_analyze = deps
    else:
        analyzing_what = "Library"
        sources_to_analyze = source_files

    # Filter the sources to analyze. We only want to analyze flight-code, so this will ignore
    # any packed record assertion or representation sources, as well as any code found under
    # a directory beginning with the name test.
    assertion_reg = re.compile(r".*build\/src\/.+\-assertion.ad[sb]$")
    representation_reg = re.compile(r".*build\/src\/.+\-representation.ad[sb]$")
    test_autocode_reg = re.compile(r".*\/test.*\/build\/src\/.+\.ad[sb]$")
    test_tester_reg = re.compile(r".*\/test.*\/.+-implementation-tester\.ad[sb]$")
    unit_test_reg = re.compile(r".*\/unit_test.*\/.+\.ad[sb]$")
    type_ranges_reg = re.compile(r".*\/build\/src\/.+_type_ranges.ad[sb]$")
    sources_to_analyze = [
        src
        for src in sources_to_analyze
        if (src.endswith(".ads") or src.endswith(".adb"))
        and not assertion_reg.match(src)
        and not representation_reg.match(src)
        and not test_autocode_reg.match(src)
        and not test_tester_reg.match(src)
        and not unit_test_reg.match(src)
        and not type_ranges_reg.match(src)
    ]

    # To support running CodePeer with "-j0" flag we cannot use the shared directory
    # to store the CodePeer output directory (which contains the CodePeer database.
    # So we make sure CodePeer always outputs to a native directory located in
    # ~/.codepeer/absolute/path/to/redo/analysis/dir. This keeps things fast
    # and allows us to use "-j0".
    from pathlib import Path

    home = str(Path.home())
    output_dir = home + os.sep + ".codepeer" + base_dir
    filesystem.safe_makedir(output_dir)

    # Copy all source and dependencies to a single analysis location. This makes
    # sure analysis is only performed on the desired files, for speed, and to not clutter the
    # log with compilation errors. There is not an easy way to tell codepeer to
    # only consider these X files for compilation. It tends to compile everything
    # it finds in a directory even if it is not in the "-files-from" switch.
    # So to better specify the exact files we want analyzed, we do this copy.
    src_dir = output_dir + os.sep + "src"
    filesystem.safe_makedir(src_dir)
    import shutil

    for dep in deps:
        shutil.copyfile(dep, src_dir + os.sep + os.path.basename(dep))

    # Write all relocated sources to analyze to file in build directory:
    relocated_sources_file = build_dir + os.sep + "sources_analyzed_relocated.txt"
    relocated_sources_to_analyze = []
    for src in sources_to_analyze:
        relocated_sources_to_analyze.append(src_dir + os.sep + os.path.basename(src))
    with open(relocated_sources_file, "w") as f:
        f.write("\n".join(relocated_sources_to_analyze))

    # Get info for forming codepeer command:
    gpr_project_file = build_target_instance.gpr_project_file().strip()
    target_path_files = build_target_instance.path_files()
    run_file = build_dir + os.sep + "run.log"
    output_file = build_dir + os.sep + "analysis.txt"
    direct = " >&2"

    # Info print:
    redo.info_print(
        "Analyzing " + analyzing_what + ":\n" + "\n".join(sources_to_analyze)
    )

    # See if there is a CodePeer yaml configuration file that determines the messages
    # and level we use for CodePeer.
    level = 2
    messages = "normal"  # max, min, normal
    switches = ""
    analyze_yaml = os.path.join(base_dir, "all.analyze.yaml")
    if os.path.exists(analyze_yaml):
        sys.stderr.write(
            "Using CodePeer configuration found in " + analyze_yaml + ".\n"
        )
        analyze_model = analyze.analyze(analyze_yaml)
        if analyze_model.messages is not None:
            messages = analyze_model.messages
        if analyze_model.level is not None:
            level = analyze_model.level
        if analyze_model.switches:
            switches = " ".join(analyze_model.switches)
    else:
        sys.stderr.write(
            "No CodePeer configuration was found at "
            + analyze_yaml
            + ". Using default configuration.\n"
        )
    sys.stderr.write(
        "Running CodePeer with switches: -level="
        + str(level)
        + " -messages="
        + messages
        + " "
        + switches
        + "\n"
    )
    sys.stderr.write("CodePeer output directory: " + output_dir + "\n")
    sys.stderr.write("Run log: " + run_file + "\n")
    sys.stderr.write(
        "^^ Tip: run 'tail -f "
        + run_file
        + "' to view the progress of a long running analysis.\n"
    )

    # If this is a 32-bit target then make sure we run CodePeer with the
    # -32bits switch enabled.
    if "32bit" in target_path_files:
        switches += " -32bits"

    # Handling for security report.
    if level > 0:
        security = " --security-report "
    # Currently disabled...
    security = ""

    # Uncomment the line below to debug AdamantMessagePatterns.xml:
    # switches += " -dbg-on patterns -show-info"

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
        + " codepeer -j0 -level "
        + str(level)
        + " -messages "
        + messages
        + " "
        + switches
        + " -f -status-codes -show-header -output-dir "
        + output_dir
        + " -output-msg -show-removed -cwe -out "
        + output_file
        + security
        + " -files-from "
        + relocated_sources_file
        + " -P "
        + gpr_project_file
        + " -XADAMANT_DIR="
        + os.environ["ADAMANT_DIR"]
        + " -XOBJECT_DIR="
        + output_dir
        + " -XSOURCE_DIRS="
        + src_dir
        + direct
    )

    # Run the compile command:
    ret, stdout, stderr = shell.try_run_command_capture_output(analyze_cmd)

    # Create a convenient link the the build/analysis directory
    # that points to the CodePeer database for this directory.
    output_link = build_dir + os.sep + "output_dir"
    try:
        os.symlink(output_dir, output_link)
    except FileExistsError:
        os.unlink(output_link)
        os.symlink(output_dir, output_link)

    # Write output to file:
    with open(run_file, "w") as f:
        f.write(stdout)
        f.write(stderr)

    # Write output to terminal:
    sys.stderr.write(stdout)
    sys.stderr.write(stderr)
    sys.stderr.write("\n-----------------------------------------------------\n")
    sys.stderr.write("---------- Analysis Output --------------------------\n")
    sys.stderr.write("-----------------------------------------------------\n")
    with open(output_file, "r") as f:
        sys.stderr.write(f.read())
    sys.stderr.write("-----------------------------------------------------\n")
    sys.stderr.write("-----------------------------------------------------\n\n")
    sys.stderr.write("CodePeer run log saved in " + run_file + "\n")
    sys.stderr.write("CodePeer analysis output saved in " + output_file + "\n")
    sys.stderr.write("CodePeer output directory located at " + output_dir + "\n")
    sys.stderr.write(
        "^^ Tip: remove this directory to reset the historical CodePeer database.\n"
    )

    return ret


# This build rule uses codepeer to analyze any code
# found in the current directory.
class build_analyze(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Define the special targets that exist everywhere...
        directory = os.path.abspath(os.path.dirname(redo_1))
        build_directory = os.path.join(directory, "build")
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except BaseException:
                targets = []
        # Find all the objects that can be built in in this build directory,
        # minus any assertion and representation objects since # those are
        # not flight packages.
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
        binaries = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and target.endswith(".elf")
            and not target.endswith("type_ranges.elf")
        ]
        if objects:
            sources, target = _get_source_files(objects)
            ret = _analyze_ada_sources(
                sources, directory, target, binary_mode=bool(binaries)
            )
            # Exit with error code if codepeer failed:
            if ret != 0:
                sys.exit(ret)
        else:
            sys.stderr.write("No source files found to analyze.\n")

    # No need to provide these for "redo what"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
