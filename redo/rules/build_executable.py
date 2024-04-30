import os.path
import sys
from shutil import move, rmtree
from util import redo
from util import error
from util import redo_arg
from util import target
from util import filesystem
from util import shell
from util import ada
from util import debug
from os import environ
from base_classes.build_rule_base import build_rule_base
from base_classes.build_target_base import build_target
from database.build_target_database import build_target_database
from database.source_database import source_database
from database.c_source_database import c_source_database
from collections import OrderedDict


# Get the build target object instance.
def _get_build_target_instance(target_name):
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


# Given a list of object files, redo-ifchange on all dependency
# object files. This function is recursive, and will redo-ifchange
# the entire dependency chain.
def _build_all_ada_object_dependencies(
    ada_object_file, source_db, build_target, custom_object_link_dir
):
    deps = []

    def sym_link_objects(objects):
        # Create a link to the object (and .ali) file in the object link directory
        # if the environment variable is set. This is a performance optimization
        # to improve Ada bind and link times.
        if custom_object_link_dir or "OBJECT_LINK_DIR" in os.environ:
            if custom_object_link_dir:
                object_link_dir = custom_object_link_dir
            else:
                object_link_dir = os.environ["OBJECT_LINK_DIR"]
            for obj in objects:
                dirname, basename, ext = redo_arg.split_full_filename(obj)
                obj_base = os.path.basename(obj)
                obj_dest = object_link_dir + os.sep + obj_base
                ali_base = basename + ".ali"
                ali = dirname + os.sep + ali_base
                ali_dest = object_link_dir + os.sep + ali_base

                if os.path.isfile(obj):
                    filesystem.safe_symlink(obj, obj_dest)
                if os.path.isfile(ali):
                    filesystem.safe_symlink(ali, ali_dest)

    def _get_immediate_dependencies(object_files):
        # Build object files that we need to find dependencies for:
        redo.redo_ifchange(object_files)

        # Read the dependency files for each object we just built and construct
        # a list of dependencies:
        dep_files = list(set([of + ".deps" for of in object_files]))
        deps = []
        for dep_file in dep_files:
            with open(dep_file, "r") as f:
                deps.extend(f.read().split("\n"))

        # Filter out files that are not Ada/C/C++ source files:
        deps = [d for d in deps if d.endswith('.ads') or d.endswith('.adb') or
                d.endswith('.c') or d.endswith('.h') or d.endswith('.hpp') or d.endswith('.cpp')]
        deps = list(set(deps))

        # Only include dependencies that exist in the database:
        dep_objects = []
        dep_packages = list(set([ada.file_name_to_package_name(dep) for dep in deps]))
        for package in dep_packages:
            objects_in_db = source_db.get_objects([package], the_target=build_target)
            if not objects_in_db:
                with c_source_database() as c_source_db:
                    objects_in_db = c_source_db.get_objects([package], the_target=build_target)
            dep_objects.extend(objects_in_db)

        return dep_objects

    def _get_all_dependencies(objects):
        objects = _get_immediate_dependencies(objects)
        # Filter out objects that are already in the deps:
        new_objects = [obj for obj in objects if obj not in deps]
        # If there are new sources, add them to the dependency list
        # and see if those sources have any dependencies:
        if new_objects:
            # Pre build the objects using gprbuild to compile them all together. This is a performance enhancement:
            # First figure out which objects need to be built. We don't want to build things that are already up
            # to date
            if not environ.get("DISABLE_PREBUILD"):
                objects_to_prebuild = redo.redo_ood(new_objects)
                if objects_to_prebuild:
                    import rules.build_object as bo

                    bo._precompile_objects(objects_to_prebuild)

            # Depend on new sources:
            redo.redo_ifchange(new_objects)
            sym_link_objects(new_objects)
            # Add them to the overall dependency list:
            deps.extend(new_objects)
            # Get the dependencies of the new sources:
            _get_all_dependencies(new_objects)

    _get_all_dependencies([ada_object_file])
    return deps


# This build rule builds an executable out of compiled object files.
# It matches Ada source files that are named either "test.adb" or
# "main.adb". Files with these names are expected to produce the
# main function for an executable in the Adamant framework.
class build_executable(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Make sure the executable is being built in the correct directory:
        if not redo_arg.in_build_bin_dir(redo_1):
            error.error_abort(
                "Executable file '"
                + redo_1
                + "' can only be built in a 'build/bin/$TARGET' directory."
            )

        # Useful vars:
        basename = redo_arg.get_base_no_ext(redo_2)
        directory = os.path.dirname(redo_2)
        build_target = redo_arg.get_target(redo_2)
        base_dir = redo_arg.get_src_dir(redo_2)
        obj_dir = os.path.join(
            base_dir, "build" + os.sep + "obj" + os.sep + build_target
        )

        # Get the build target instance:
        build_target = redo_arg.get_target(redo_2)
        build_target_instance, build_target_file = _get_build_target_instance(
            build_target
        )

        # Determine our local module:
        local_module = os.path.join(
            redo_arg.get_src_dir(redo_2),
            "build" + os.sep + "obj" + os.sep + build_target + os.sep + basename + ".o",
        )

        # Depend on dependencies:
        deps = [local_module, build_target_file, __file__]
        redo.redo_ifchange(deps)

        # Build all the objects that are required for this executable:
        deps_to_write = deps
        with source_database() as db:
            deps_to_write.extend(
                _build_all_ada_object_dependencies(
                    local_module, db, build_target, obj_dir
                )
            )

        # Form temporary files paths:
        build_dir, base_name = redo_arg.split_redo_arg(redo_2)
        temp_dir = os.path.join(build_dir, base_name + "-temp")
        temp_file = os.path.join(temp_dir, base_name + ".elf")

        filesystem.safe_makedir(temp_dir)
        # Get the source files associated with this object:
        # First, let's assume this object can be built from Ada
        # source code.
        package_name = ada.file_name_to_package_name(redo_2)
        source_files = None
        with source_database() as db:
            source_files = db.try_get_sources(package_name)

        # GCC wants an adb file if it exists, and an
        # ads file if an adb doesn't exist. Let's
        # give GCC what it wants.
        source_to_compile = None
        for source_file in source_files:
            _, source_extension = os.path.splitext(source_file)
            if source_extension == ".adb":
                source_to_compile = source_file
                break
            elif source_extension == ".ads":
                source_to_compile = source_file

        # Make sure we found a source to compile. We should never NOT find one:
        assert source_to_compile
        assert source_to_compile.endswith(".adb")

        source_dir = os.path.dirname(source_to_compile)

        # Find any C objects that need to be linked against so that we can
        # tell GPRbuild to link with them.
        with c_source_database() as db:
            c_objects = db.get_all_objects(build_target)
        existing_c_objects = [ofile for ofile in c_objects if os.path.isfile(ofile)]
        c_source_path = list(
            set([redo_arg.get_src_dir(obj) for obj in existing_c_objects])
        )

        # Sym link any c objects into the executable object directory, since
        # these would not have gotten symlinked like all the ada objects
        # above.
        for obj in existing_c_objects:
            obj_base = os.path.basename(obj)
            obj_dest = obj_dir + os.sep + obj_base
            filesystem.safe_symlink(obj, obj_dest)

        # Set the verbosity level to low if debug is on:
        verbosity = ""
        direct = " >/dev/null"
        if "DEBUG" in os.environ and os.environ["DEBUG"]:
            verbosity = " -vl"
            direct = " >&2"

        # Form the gprbuild commands:
        filesystem.safe_makedir(directory)
        gpr_project_file = build_target_instance.gpr_project_file().strip()
        gpr_project_file_dir = os.path.dirname(gpr_project_file)
        gprbuild_flags = build_target_instance.gprbuild_flags()
        # dep_dirs = list(set([os.path.dirname(f) for f in deps]))
        gprbuild_prefix = (
            "gprbuild"
            + verbosity
            + " -ws -f -aP "
            + os.environ["ADAMANT_DIR"]
            + os.sep
            + "redo"
            + os.sep
            + "targets"
            + os.sep
            + "gpr"
            + " -P "
            + gpr_project_file
        )
        gprbuild_suffix = (
            source_to_compile
            + " -XADAMANT_DIR="
            + os.environ["ADAMANT_DIR"]
            + " -XOBJECT_DIR="
            + obj_dir
            + " -XEXEC_DIR="
            + temp_dir
            + " -XSOURCE_DIRS="
            + source_dir
            + (("," + ",".join(c_source_path)) if c_source_path else "")
            + ((" " + gprbuild_flags.strip()) if gprbuild_flags else "")
            + direct
        )
        bind_command = gprbuild_prefix + " -b " + gprbuild_suffix
        link_command = gprbuild_prefix + " -l " + gprbuild_suffix

        # Make the build directory:
        filesystem.safe_makedir(directory)

        # Determine source file dependencies:
        src_deps_to_write = []
        for obj in deps_to_write:
            if obj.endswith(".o"):
                with open(obj + ".deps", "r") as f:
                    src_deps_to_write.extend(f.read().split("\n"))
        src_deps_to_write = list(OrderedDict.fromkeys(src_deps_to_write))

        # Write dependencies:
        obj_deps_file = temp_file + ".obj_deps"
        src_deps_file = temp_file + ".deps"
        with open(obj_deps_file, "w") as f:
            f.write("\n".join(deps_to_write))
        with open(src_deps_file, "w") as f:
            f.write("\n".join(src_deps_to_write))

        # Get debug flag
        do_debug = False
        if "DEBUG" in os.environ and os.environ["DEBUG"]:
            do_debug = True

        # Run the link and bind commands in .gpr directory:
        cwd = os.getcwd()
        os.chdir(gpr_project_file_dir)
        shell.run_command(bind_command, debug=do_debug)
        shell.run_command(link_command, debug=do_debug)
        os.chdir(cwd)

        # Move the temporary stuff over to the final location:
        debug.debug_print("mv " + temp_file + " " + redo_3)
        move(temp_file, redo_3)
        for f in os.listdir(temp_dir):
            move(os.path.join(temp_dir, f), os.path.join(build_dir, f))
        rmtree(temp_dir)

        # Create binary meta data report file:
        if (
            "CREATE_BUILD_ARTIFACTS" in os.environ
            and os.environ["CREATE_BUILD_ARTIFACTS"]
        ):
            meta_file = redo_1 + ".txt"
            from util import binary_meta

            try:
                string = binary_meta.get_report(
                    redo_1,
                    main_object_filename=local_module,
                    redo_temp_filename=redo_3,
                    build_target_instance=build_target_instance,
                )
                if string:
                    with open(meta_file, "w") as f:
                        f.write(string)
            except BaseException as e:
                import traceback

                sys.stderr.write(
                    "Warning: creating binary meta data file '"
                    + meta_file
                    + "' for '"
                    + redo_1
                    + "' failed with error:\n"
                    + str(e)
                    + "\n"
                )
                sys.stderr.write(traceback.format_exc() + "\n")

    # Match source code named test.adb or main.adb
    def input_file_regex(self):
        return [r".*\/test\.adb$", r".*\/main\.adb$", r".*\/.*_type_ranges.adb"]

    # Output binaries always go in the build/bin folder and the
    # filename is appended with ".elf"
    def output_filename(self, input_filename):
        base = redo_arg.get_base_no_ext(input_filename)
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(
            directory,
            "build"
            + os.sep
            + "bin"
            + os.sep
            + target.get_default_target()
            + os.sep
            + base
            + ".elf",
        )
