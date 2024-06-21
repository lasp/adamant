import os.path
from util import redo
from util import error
from util import target
from util import filesystem
from util import redo_arg
from util import shell
from base_classes.build_rule_base import build_rule_base
from database.build_target_database import build_target_database
from database.utility_database import utility_database
from base_classes.build_target_base import build_target
import sys
import re


# Private helper functions:
def _get_build_target_instance(target_name):
    """Get the build target object instance."""
    try:
        with build_target_database() as db:
            instance, filename = db.get_build_target_instance(target_name)
    except KeyError:
        error.error_abort(
            "No rules found for build source for target '" + target_name + "'."
        )
    try:
        instance.ada_compiler_depends_on()
        return build_target(instance), filename
    except AttributeError:
        return instance, filename


def _get_include_list():
    """Return the includes a list."""
    build_path = None
    with utility_database() as db:
        build_path = db.get_source_build_path()
    return build_path


class build_metric(build_rule_base):
    """
    This build rule is capable of using gnatmetric to generate a
    metric file for any ada source code, object, or executable.
    """
    def _build(self, redo_1, redo_2, redo_3):
        if not redo_arg.in_build_metric_dir(redo_1):
            error.error_abort(
                "Metric file '"
                + redo_1
                + "' can only be built in a 'build/metric' directory."
            )
        if not redo_1.endswith(".txt"):
            error.error_abort(
                "File '"
                + redo_1
                + "' does not have a .txt extension. Only text files can be built in 'build/metric'."
            )

        redo_no_txt = ".".join(redo_2.split(".")[:-1])
        redo_no_txt_base = os.path.basename(redo_no_txt)
        # Make sure the file that we want to compute a metric for is an object, src_file or elf
        if not (
            redo_no_txt.endswith(".ads")
            or redo_no_txt.endswith(".adb")
            or redo_no_txt.endswith(".elf")
            or redo_no_txt.endswith(".o")
        ):
            error.error_abort(
                "File '"
                + redo_no_txt_base
                + "' is not valid Ada source, an object, or executable (ELF) filename."
            )

        # Get the build target instance:
        build_target = redo_arg.get_target(redo_2)
        build_target_instance, build_target_file = _get_build_target_instance(
            build_target
        )

        # Useful directories and strings:
        dirname, basename, ext = redo_arg.split_full_filename(redo_no_txt)
        root_dir = redo_arg.get_src_dir(redo_2)
        metric_dir = os.path.join(
            root_dir, "build" + os.sep + "metric" + os.sep + build_target
        )
        detailed_metric_dir = os.path.join(metric_dir, redo_no_txt_base + ".metrix")  # codespell:ignore metrix
        cargs_string = ""
        filesystem.safe_makedir(detailed_metric_dir)

        # Get the gnatmetric info from the target:
        metric_prefix, flags = build_target_instance.gnatmetric_info(
            target=build_target
        )

        # Form the gnatmetric command:
        metric_cmd = metric_prefix.strip() + "gnatmetric"
        includes = _get_include_list()
        include_string = "-I" + " -I".join(includes)
        cargs_string = flags + " " + include_string
        cmd = metric_cmd + " -d " + detailed_metric_dir

        # Add cargs string to command:
        cmd += " " + cargs_string

        def run_cmd(cmd, source, string=None):
            """
            Helper which runs the gnat metric command in a way that allows
            us to work around a problem when there is a large number of source
            files.

            Sometimes the number of arguments to gnatmetric can exceed the OS
            limit. To avoid this, we put all the source files in a temporary
            file and cat them into the command using "xargs".
            """
            def do_run_cmd(cmd, string=None):
                """Helper which actually runs the command."""
                if string:
                    sys.stdout.write(string + "\n")
                sys.stdout.write("Sources analyzed:\n")
                sys.stdout.write(str(source) + "\n\n")
                sys.stdout.write("Command:\n")
                sys.stdout.write("$ " + cmd + "\n\n")
                sys.stdout.flush()
                shell.run_command(cmd)

            import tempfile

            fd, path = tempfile.mkstemp()
            try:
                with os.fdopen(fd, "w") as tmp:
                    # Write source files to temp file
                    tmp.write(source)
                # Construct the metrics command:
                cmd = "cat " + path + " | xargs " + cmd
                do_run_cmd(cmd, string)
            finally:
                os.remove(path)

        # Construct the metrics file:
        if ext.endswith(".ads") or ext.endswith(".adb"):
            # We must depend on the object. This is a quick and dirty way to make sure
            # all the autocode is built first.
            # Note: we "redo" the object file instead of "redo-ifchange" to force a rebuild.
            # This will refresh the symlinks in the cache, making sure things run fast and
            # don't error.
            obj_file = os.path.join(
                root_dir,
                "build"
                + os.sep
                + "obj"
                + os.sep
                + build_target
                + os.sep
                + basename
                + ".o",
            )
            redo.redo(obj_file)

            # Find and depend on the source file:
            source = os.path.join(root_dir, redo_no_txt_base)
            if not os.path.isfile(source):
                source = os.path.join(
                    root_dir, "build" + os.sep + "src" + os.sep + redo_no_txt_base
                )
            redo.redo_ifchange(source)

            # Construct the metrics command:
            run_cmd(cmd, source)

        elif ext.endswith(".o"):
            # We must depend on the object. This is a quick and dirty way to make sure
            # all the autocode is built first.
            # Note: we "redo" the object file instead of "redo-ifchange" to force a rebuild.
            # This will refresh the symlinks in the cache, making sure things run fast and
            # don't error.
            obj_file = os.path.join(
                root_dir,
                "build"
                + os.sep
                + "obj"
                + os.sep
                + build_target
                + os.sep
                + basename
                + ".o",
            )
            redo.redo(obj_file)

            # The metric for a .o is all the code that makes up that object and any
            # specs (.ads) it depends on. We expect that a .deps file is created for
            # the object. Use this file to compute the metrics:
            deps_file = obj_file + ".deps"
            with open(deps_file, "r") as f:
                deps = f.read()
            deps = [
                d for d in deps.split("\n") if d.endswith(".ads") or d.endswith(".adb")
            ]
            deps = list(set(deps))

            # Separate the autocoded deps from the hand coded deps:
            reg = re.compile(r".*build/src/.*\.ad[sb]$")
            autocode_deps = []
            handcode_deps = []
            for d in deps:
                if reg.match(d):
                    autocode_deps.append(d)
                else:
                    handcode_deps.append(d)

            # Construct the metrics command:
            if autocode_deps:
                run_cmd(cmd, " ".join(autocode_deps), "Autocoded Metrics:")
            if handcode_deps:
                run_cmd(cmd, " ".join(handcode_deps), "\nHandcoded Metrics:")
            if deps:
                run_cmd(cmd, " ".join(deps), "\nTotal Metrics:")
        elif ext.endswith(".elf"):
            # We must depend on the executable. This is a quick and dirty way to make sure
            # all the autocode is built first.
            # Note: we "redo" the object file instead of "redo-ifchange" to force a rebuild.
            # This will refresh the symlinks in the cache, making sure things run fast and
            # don't error.
            elf_file = os.path.join(
                root_dir,
                "build"
                + os.sep
                + "bin"
                + os.sep
                + build_target
                + os.sep
                + basename
                + ".elf",
            )
            redo.redo(elf_file)

            # The metric for a .elf includes all code that is linked. The easiest way
            # to do this is to make sure the elf is built first, then run gnatmetric
            # on all its dependencies.
            # elf_file = redo_2.replace(os.sep + "metric" + os.sep, os.sep + "bin" + os.sep)[:-4]
            # redo.redo_ifchange(elf_file)

            # The metric for a .o is all the code that makes up that object and any
            # specs (.ads) it depends on. We expect that a .deps file is created for
            # the object. Use this file to compute the metrics:
            deps_file = elf_file + ".deps"
            with open(deps_file, "r") as f:
                deps = f.read()
            deps = [
                d for d in deps.split("\n") if d.endswith(".ads") or d.endswith(".adb")
            ]
            deps = list(set(deps))

            # Separate the autocoded deps from the hand coded deps:
            reg = re.compile(r".*build/src/.*\.ad[sb]$")
            autocode_deps = []
            handcode_deps = []
            for d in deps:
                if reg.match(d):
                    autocode_deps.append(d)
                else:
                    handcode_deps.append(d)

            # Construct the metrics command:
            if autocode_deps:
                run_cmd(cmd, " ".join(autocode_deps), "Autocoded Metrics:")
            if handcode_deps:
                run_cmd(cmd, " ".join(handcode_deps), "\nHandcoded Metrics:")
            if deps:
                run_cmd(cmd, " ".join(deps), "\nTotal Metrics:")
        else:
            assert False, "Should never get here. This is a bug :("

    def input_file_regex(self):
        return [r".*\.ad[sb]$", r".*\.o$", r".*\.elf$"]

    def output_filename(self, input_filename):
        base = os.path.basename(input_filename)
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(
            directory,
            "build"
            + os.sep
            + "metric"
            + os.sep
            + target.get_default_target()
            + os.sep
            + base
            + ".txt",
        )
