import os.path
from shutil import move
from shutil import rmtree
from util import error
from util import target
from util import filesystem
from util import redo_arg
from util import shell
from util import debug
from base_classes.build_rule_base import build_rule_base
from rules.build_object import _build_all_c_dependencies, _get_build_target_instance


# This function compiles a c object file from a given
# source file.
def _generate_bindings(redo_1, redo_2, redo_3, source_file, c_source_db):

    # First build all the dependencies for this source file and for the include
    # string for those dependencies:
    build_target_instance, _ = _get_build_target_instance(target.get_default_target())
    deps = _build_all_c_dependencies([source_file], c_source_db, build_target_instance=build_target_instance)
    dep_dirs = list(set([os.path.dirname(dep) for dep in deps]))
    include_str = "-I" + " -I".join(dep_dirs)

    # Should we use gcc or g++ for bindings generation
    compiler = "gcc"
    if source_file.endswith("hpp"):
        compiler = "g++"

    # Generate the compilation command that dumps the bindings, ie:
    #    gcc -c -fdump-ada-spec -C c_lib.h
    compile_cmd = (
        compiler + " "
        + "-c "
        + "-fdump-ada-spec "
        + "-C "
        + source_file
        + " " + include_str
    )

    # Run the bindings command in templates directory so the file
    # gets created there:
    cwd = os.getcwd()
    template_temp_dir = redo_arg.get_build_dir(redo_1) + os.sep + "template" + os.sep + "temp"
    filesystem.safe_makedir(template_temp_dir)
    os.chdir(template_temp_dir)
    shell.run_command_suppress_output(compile_cmd)
    os.chdir(cwd)

    # Move the temporary binding over to the final location:
    temp_output = os.path.join(template_temp_dir, os.path.basename(redo_1))
    debug.debug_print("mv " + temp_output + " " + redo_3)
    move(temp_output, redo_3)
    rmtree(template_temp_dir)


# This build rule is capable of compiling object files from either
# Ada or C/C++ source. It matches on any source file in the system
# and produces objects of the same name as the source file in the
# build/obj directory. If the source file is found in the Ada source
# database, then it is compiled as Ada source. If the file is not found
# in the Ada source database, but is found in the C/C++ source database
# it is compiled as C source.
class build_bindings(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Make sure the binding is being built in the correct directory:
        if not redo_arg.in_build_template_dir(redo_1):
            error.error_abort(
                "Object file '"
                + redo_1
                + "' can only be built in a 'build/template' directory."
            )

        # If no source files were found, then maybe this object
        # is built from C or C++ source.
        # Try to grab C or C++ source files based on the
        # basename.
        assert redo_1.endswith("_h.ads") or redo_1.endswith("_hpp.ads")
        basename = redo_arg.get_base_no_ext(redo_2)
        if basename.endswith("_h"):
            sourcename = basename[:-2]  # remove "_h"
        elif basename.endswith("_hpp"):
            sourcename = basename[:-4]  # remove "_hpp"
        else:
            assert False
        from database.c_source_database import c_source_database
        import unqlite

        with c_source_database() as db:
            # A lot of the time a c database won't even exist, so we put a try/except
            # block to catch this case and display the appropriate error message.
            try:
                source_files = db.try_get_source(sourcename)
            except unqlite.UnQLiteError:
                error.error_abort(
                    "No C/C++ source files were found to generate bindings in '" + redo_1 + "'. (UnQLiteError)"
                )
            except TypeError:
                error.error_abort(
                    "No C/C++ source files were found to generate bindings in '" + redo_1 + "'. (TypeError)"
                )

            # Indeed this is C source, compile it.
            if source_files:
                # We only care about headers
                source_files = [source for source in source_files if source.endswith(".h") or source.endswith(".hpp")]
                if len(source_files) > 0:
                    assert len(source_files) == 1, str(source_files)
                    return _generate_bindings(
                        redo_1, redo_2, redo_3, source_files[0], db
                    )

        # If we still have not found source files to build this
        # object, then we are out of luck. Alert the user.
        error.error_abort("No C/C++ source files were found to generate bindings in '" + redo_1 + "'.")

    # Match any C header source file
    def input_file_regex(self):
        return [r"^((?!template/).)*\.hp?p?$"]

    # Output object files will always be stored with the same name
    # as their source package, and in the build/obj directory.
    def output_filename(self, input_filename):
        _, base, ext = redo_arg.split_full_filename(input_filename)
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(
            directory,
            "build"
            + os.sep
            + "template"
            + os.sep
            + base
            + "_" + ext[1:]
            + ".ads",
        )
