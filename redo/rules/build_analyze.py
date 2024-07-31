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

    # We cannot use "fast" compilation when running GNAT SAS, since GNAT SAS wants to analyze both
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

    # So we make sure GNAT SAS always outputs to a native directory located in
    # ~/.gnatsas/absolute/path/to/redo/analysis/dir. This keeps things fast.
    from pathlib import Path

    home = str(Path.home())
    output_dir = home + os.sep + ".gnatsas" + base_dir
    filesystem.safe_makedir(output_dir)

    # Copy all source and dependencies to a single analysis location. This is a
    # simple way to ensure analysis is only performed on the desired files.
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

    # Get info for forming gnatsas command:
    gpr_project_file = build_target_instance.gpr_project_file().strip()

    # Info print:
    redo.info_print(
        "Analyzing " + analyzing_what + ":\n" + "\n".join(sources_to_analyze)
    )

    # Function to modify the contents of a gpr file for use with
    # gnatsas
    def modify_contents(gpr_contents, gpr_file_name):
        # Split the contents into lines
        lines = gpr_contents.split('\n')

        # Set flag to find if 'Source_Dirs' line was found
        source_dirs_line_exists = False
        source_dirs_line = '   for Source_Dirs use ("./**");'

        # Replace any Source_Dirs declaration with the one for gnatsas
        for i, line in enumerate(lines):
            # Replace "for Source_Dirs use" line with new line
            if line.strip().startswith("for Source_Dirs use"):
                lines[i] = source_dirs_line
                source_dirs_line_exists = True
                break

        # If there is no 'Source_Dirs' line, add it after 'project' line
        if not source_dirs_line_exists:
            for i, line in enumerate(lines):
                if line.strip().startswith("project "):
                    lines.insert(i+1, source_dirs_line)
                    break

        # This is a bit hacky, but should work for current Adamant gpr files
        # for native targets.
        def contains_linux_or_native(input_string):
            # Convert the input string to lower case
            input_string_lower = input_string.lower()
            return "linux" in input_string_lower or "native" in input_string_lower

        # Insert gnatsas target near the bottom
        if contains_linux_or_native(lines[0]):
            length = len(lines)
            for i, line in enumerate(reversed(lines)):
                # Insert target line before end line
                if line.strip().startswith("end "):
                    lines.insert(length-i-1, '   for Target use "codepeer";')
                    break

        # Join the modified lines back into a single string
        return "\n".join(lines)

    # Open the .gpr file for the current target and read its
    # contents. We are going to use this as the base for the
    # gnatsas .gpr file, but with some modifications.
    gpr_contents = None
    try:
        with open(gpr_project_file, 'r') as f:
            gpr_contents = f.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"The file '{gpr_project_file}' does not exist.")

    # Make the appropriate modifications. This seems hacky, but
    # should work for any of the gpr files provided with Adamant
    # without issue.
    new_gpr_contents = modify_contents(gpr_contents, gpr_project_file)

    # Open the file in write mode
    gnatsas_gpr_file = os.path.join(src_dir, os.path.basename(gpr_project_file))
    with open(gnatsas_gpr_file, 'w') as f:
        # Write the modified contents to the file
        f.write(new_gpr_contents)

    # Run GNAT SAS analysis
    output_dir = os.path.join(src_dir, "reports")
    filesystem.safe_makedir(output_dir)
    analyze_out_file = os.path.join(output_dir, "analyze.txt")
    suffix = " 2>&1 | tee " + analyze_out_file + " 1>&2"
    analyze_cmd = "gnatsas analyze -j0 -P" + gnatsas_gpr_file + suffix
    ret = shell.try_run_command(analyze_cmd)

    # Make CSV report
    csv_out_file = os.path.join(output_dir, "report.csv")
    csv_cmd = "gnatsas report csv -P" + gnatsas_gpr_file + " --out " + csv_out_file + suffix
    ret = shell.try_run_command(csv_cmd)

    # Make html report
    # html_out_file = os.path.join(src_dir, "gnathub" + os.sep + "html-report" + os.sep + "index.html")
    # html_cmd = "gnatsas report html -P" + gnatsas_gpr_file + suffix
    # ret = shell.try_run_command(html_cmd)

    # Make security report
    security_out_file = os.path.join(output_dir, "security.html")
    security_cmd = "gnatsas report security -P" + gnatsas_gpr_file + " --out " + security_out_file + suffix
    ret = shell.try_run_command(security_cmd)

    # Make text report and print to screen
    report_out_file = os.path.join(output_dir, "report.txt")
    suffix = " 2>&1 | tee " + report_out_file + " 1>&2"
    report_cmd = "gnatsas report -P" + gnatsas_gpr_file + suffix

    # Write output to terminal:
    sys.stderr.write("\n-----------------------------------------------------\n")
    sys.stderr.write("---------- Analysis Output --------------------------\n")
    sys.stderr.write("-----------------------------------------------------\n")
    ret = shell.try_run_command(report_cmd)
    sys.stderr.write("-----------------------------------------------------\n")
    sys.stderr.write("-----------------------------------------------------\n\n")
    sys.stderr.write("GNAT SAS output directory located at " + output_dir + "\n")
    sys.stderr.write("GNAT SAS run log saved in " + analyze_out_file + "\n")
    sys.stderr.write("GNAT SAS analysis text output saved in " + report_out_file + "\n")
    sys.stderr.write("GNAT SAS analysis CSV output saved in " + csv_out_file + "\n")
    # sys.stderr.write("GNAT SAS analysis HTML output saved in " + html_out_file + "\n")
    sys.stderr.write("GNAT SAS security report output saved in " + security_out_file + "\n")

    return ret


class build_analyze(build_rule_base):
    """
    This build rule uses gnatsas to analyze any code
    found in the current directory.
    """
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
            # Exit with error code if gnatsas failed:
            if ret != 0:
                sys.exit(ret)
        else:
            sys.stderr.write("No source files found to analyze.\n")

    # No need to provide these for "redo what"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
