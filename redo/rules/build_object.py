import os.path
from shutil import move
from shutil import rmtree
from os import environ
from util import redo
from util import error
from util import ada
from util import target
from util import filesystem
from util import redo_arg
from util import shell
from util import debug
from base_classes.build_rule_base import build_rule_base
from base_classes.build_target_base import build_target
from database.source_database import source_database
from database.c_source_database import c_source_database
from database.build_target_database import build_target_database


# Private helper functions:
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


# Given a list of source files, redo-ifchange on all dependency
# source files. This function is recursive, and will redo-ifchange
# the entire dependency chain.
def _build_all_ada_dependencies(ada_source_files, source_db):
    deps = []

    #
    # By default, the build system assumes that any "with" dependency
    # found in a file is a dependency on both the .ads (spec) and .adb
    # (body) for that with'ed package. This is a conservative approximation
    # to the true dependency rules, which can be found here:
    #
    # https://gcc.gnu.org/onlinedocs/gcc-4.9.2/gnat_ugn/Source-Dependencies.html
    #
    # This approach, while always producing correctly compiled code, is
    # non-optimal and may require the recompilation of source code and
    # object code that in reality doesn't actually need to be recompiled.
    # Sometimes this can be slow. To deal with this problem, the following
    # flag was created.
    #
    # When enabled, this flag will only assume dependencies on spec files (.ads).
    # While this is incorrect, about 98% of the time it will be correct. In this
    # case recompilation performance will be much improved. In the 2% chance that
    # the compilation fails, a warning will be displayed to the user by gcc telling
    # them that a file doesn't exist. In this case the user can build this source
    # manually using redo, and continue on their way, or switch the flag off
    # temporarily before switching it back on for the next compile.
    #
    # NOTE about the above: Fast compile is now enabled by default, since now
    # have the capability to detect when we need to depend on .adb files. To
    # disabled fast compile you need to set the SAFE_COMPILE via the commandline.
    #
    # NOTE there are certain times this must be turned off, such as when performing
    # coverage analysis or verifying SPARK proofs with GNATprove. In both of these
    # cases the build system automatically switches fast compilation off to support
    # these functions.
    #
    fast_compile = True
    if environ.get("SAFE_COMPILE"):
        fast_compile = False

    def _get_immediate_dependencies(source_files):
        # Get dependencies for these source files:
        dependency_packages = []
        for source_file in source_files:
            if source_file.endswith(".ads") or source_file.endswith(".adb"):
                dependency_packages.extend(ada.get_source_dependencies(source_file))
        dependency_packages = filter(bool, dependency_packages)
        dependency_packages = list(set(dependency_packages))

        # Get all the source files that correspond to these packages:
        sources = source_db.try_get_sources(dependency_packages)

        # Split up adb and ads sources:
        ads_sources = [source for source in sources if source.endswith(".ads")]
        adb_sources = [source for source in sources if source.endswith(".adb")]

        return ads_sources, adb_sources

    def _get_all_dependencies(source_files):
        sources = _get_immediate_dependencies(source_files)
        # Filter out sources that are already in the deps:
        new_ads_sources, new_adb_sources = [
            source for source in sources if source not in deps
        ]

        # Special handling to make sure we build C-objects for which we have
        # Ada bindings: In most cases if Ada calls into C/C++ or vice versa
        # there is no way of "discovering" that that dependency exists. However,
        # in this special case we can follow a convention to cover the common case
        # where a user has generated Ada bindings for .h or .hpp file. These generated
        # bindings always create a filename of the form <c_header_name>_h.ads or
        # <cpp_header_name>_hpp.ads. Using this we can assume that these .ads files
        # depend on the .h and .hpp files they bind. The code below recognizes this
        # filenaming convention and will claim the binded C/C++ headers as
        # dependencies of the Ada binding .ads files. By declaring this dependency,
        # this will ensure the C/C++ object will be compiled and be included at
        # link time.
        c_sources = []
        for source in new_ads_sources:
            if source.endswith("_h.ads"):
                with c_source_database() as db:
                    c_sources.extend(db.get_sources([os.path.basename(source[:-6])]))
            if source.endswith("_hpp.ads"):
                with c_source_database() as db:
                    c_sources.extend(db.get_sources([os.path.basename(source[:-8])]))
        if c_sources:
            redo.redo_ifchange(c_sources)
            deps.extend(c_sources)

        if new_ads_sources:
            if fast_compile:
                # Depend on new sources:
                redo.redo_ifchange(new_ads_sources)

                # Add them to the overall dependency list:
                deps.extend(new_ads_sources)

                # If we are in fast_compile mode, we have not yet depended on any
                # adb files. We need to do this now that the ads files are created.
                #
                # Figuring out which adb's we depend on is a bit trickier. It is according to
                # these rules:
                #   https://gcc.gnu.org/onlinedocs/gcc-4.9.2/gnat_ugn/Source-Dependencies.html
                # which is implemented by the should_depend_on_adb function call below.
                required_adb_sources = []
                for ads_source in new_ads_sources:
                    if ada.should_depend_on_adb(ads_source):
                        # Find the matching adb source in the adb sources list. It could
                        # be in a different directory than the ads file, so we need to actually
                        # look it up.
                        ads_basename = os.path.basename(ads_source)
                        adb_basename = ads_basename[:-1] + "b"
                        for adb_source in new_adb_sources:
                            if adb_source.endswith(adb_basename):
                                required_adb_sources.append(adb_source)

                # Depend on adb sources:
                redo.redo_ifchange(required_adb_sources)

                # Add them to the overall dependency list:
                deps.extend(required_adb_sources)

                # Add the adbs to the new_sources list:
                new_sources = new_ads_sources + required_adb_sources

            else:
                # We are not in fast compile mode. We will conservatively depend on all ads
                # and all adb files
                new_sources = new_ads_sources + new_adb_sources

                # Depend on new sources:
                redo.redo_ifchange(new_sources)

                # Add them to the overall dependency list:
                deps.extend(new_sources)

            # Get the dependencies of the new sources:
            _get_all_dependencies(new_sources)

    _get_all_dependencies(ada_source_files)
    return deps


# Given a C/C++ source file, use g++ to determine the dependencies that the
# source file has.
def get_c_source_dependencies(source_file, build_target_instance, c_source_db=None):

    def _extract_from_deps_file(dependency_file, object_file):
        dependencies = []
        found_object = False

        with open(dependency_file, 'r') as dfile:
            for line in dfile:
                if line.startswith(object_file + ":"):
                    found_object = True
                    dependencies.extend(line.split()[1:])
                elif found_object:
                    if ':' in line:
                        break
                    else:
                        dependencies.extend(line.strip().split())

        dependencies = [dep for dep in dependencies if dep[0] != '\\']
        return dependencies

    # Get dependencies for this source files:
    _, source_extension = os.path.splitext(source_file)

    # Get the C-based includes in the build path:
    if c_source_db:
        includes = "-I" + " -I".join(c_source_db.get_all_source_dirs())
    else:
        with c_source_database() as db:
            includes = "-I" + " -I".join(db.get_all_source_dirs())

    # Run g++ -MM to generate dependencies for a source file in a .d output:
    if source_file.endswith(".cpp"):
        compiler = "g++"
    else:
        compiler = "gcc"
    obj_file = redo_arg.src_file_to_obj_file(source_file, build_target_instance.name)
    dep_output_file = os.path.join(os.path.dirname(obj_file), os.path.basename(source_file)) + ".d"
    filesystem.safe_makedir(os.path.dirname(dep_output_file))
    compiler_prefix, _ = build_target_instance.gnatmetric_info()
    gcc_m_command = compiler_prefix + compiler + " " + source_file + " -MM -MG -MF " + dep_output_file + " " + includes
    shell.run_command(gcc_m_command)

    # Open the dependency file and parse it to get out the dependencies:
    return _extract_from_deps_file(dep_output_file, os.path.basename(obj_file))


def _build_all_c_dependencies(
    c_source_files, c_source_db, build_target_instance
):
    deps = []

    def _get_immediate_dependencies(source_files):
        all_deps = []
        for source_file in source_files:
            # Get dependencies for this source file. Ignore assembly files.
            if not source_file.endswith(".S") and not source_file.endswith(".s"):
                all_deps.extend(get_c_source_dependencies(source_file, build_target_instance, c_source_db))
        return list(set(all_deps))

    def _get_all_dependencies(source_files):
        sources = _get_immediate_dependencies(source_files)
        # Filter out sources that are already in the deps:
        new_sources = [source for source in sources if source not in deps]
        # If there are new sources, add them to the dependency list
        # and see if those sources have any dependencies:
        if new_sources:
            # Depend on new sources:
            redo.redo_ifchange(new_sources)
            # Add them to the overall dependency list:
            deps.extend(new_sources)
            # Get the dependencies of the new sources:
            _get_all_dependencies(new_sources)

    _get_all_dependencies(c_source_files)
    return deps


def _run_gprbuild_command(build_target_instance, sources_to_compile, source_dependencies, object_dir):
    # Form the gprbuild command:
    gpr_project_file = build_target_instance.gpr_project_file().strip()
    gpr_project_file_dir = os.path.dirname(gpr_project_file)
    gprbuild_flags = build_target_instance.gprbuild_flags()
    dep_dirs = list(set([os.path.dirname(f) for f in source_dependencies]))

    # Set the verbosity level to low if debug is on:
    verbosity = ""
    direct = " >/dev/null"
    if "DEBUG" in os.environ and os.environ["DEBUG"]:
        verbosity = " -vl"
        direct = " >&2"

    # Construct the command string:
    sources_to_compile_str = " ".join(sources_to_compile)
    compile_cmd = (
        "gprbuild"
        + verbosity
        + " -ws -f -u --relocate-build-tree -aP "
        + os.environ["ADAMANT_DIR"]
        + os.sep
        + "redo"
        + os.sep
        + "targets"
        + os.sep
        + "gpr"
        + " -P "
        + gpr_project_file
        + " -c "
        + sources_to_compile_str
        + " -XADAMANT_DIR="
        + os.environ["ADAMANT_DIR"]
        + " -XOBJECT_DIR="
        + object_dir
        + " -XSOURCE_DIRS="
        + ",".join(dep_dirs)
        + ((" " + gprbuild_flags.strip()) if gprbuild_flags else "")
        + direct
    )

    # Run the compile command in .gpr directory:
    cwd = os.getcwd()
    os.chdir(gpr_project_file_dir)
    shell.run_command(compile_cmd)
    os.chdir(cwd)


####################################################
# Speed optimization functions:
####################################################

# Returns the source file to compile for a given object:
def _get_object_sources(object_file):
    # Make sure the object is being built in the correct directory:
    if not redo_arg.in_build_obj_dir(object_file):
        error.error_abort(
            "Object file '"
            + object_file
            + "' can only be built in a 'build/obj/$TARGET' directory."
        )

    # Get the source files associated with this object:
    # First, let's assume this object can be built from Ada
    # source code.
    package_name = ada.file_name_to_package_name(object_file)
    source_files = None
    with source_database() as db:
        source_files = db.try_get_sources(package_name)

    if source_files:
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

    else:
        # No Ada source found, let's look for C/C++ source
        from database.c_source_database import c_source_database
        with c_source_database() as db:
            source_files = db.try_get_sources(package_name)

        # GCC wants an c or cpp file if it exists, since we cannot
        # build an object from just a .h file.
        source_to_compile = None
        source_to_compile_extension = None
        for source_file in source_files:
            _, source_to_compile_extension = os.path.splitext(source_file)
            if source_to_compile_extension in [".c", ".cpp", ".S", ".s"]:
                source_to_compile = source_file
                break

    if not source_to_compile:
        error.error_abort("No Ada source files were found to build '" + object_file + "'.")

    # Make sure we found a source to compile. We should never NOT find one:
    assert source_to_compile
    return source_to_compile, source_files


def _precompile_objects(object_files):
    # Form temporary files paths:
    temp_object_dir = os.environ["OBJECT_PRE_BUILD_DIR"]

    # Make sure that the build target is the same for all object files. This should be enforced by the
    # build system itself.
    build_target = redo_arg.get_target(object_files[0])
    for obj_file in object_files:
        assert build_target == redo_arg.get_target(
            obj_file
        ), "All build object file must have same build target!"

    # Get the build target instance:
    build_target_instance, build_target_file = _get_build_target_instance(build_target)

    # Depend on this file and the target file:
    redo.redo_ifchange([build_target_file, __file__])

    # Create the source files we need to compile to build these objects:
    # source_files = []
    sources_to_compile = []
    all_deps = []
    for object_file in object_files:
        # Get immediate sources for object:
        source_to_compile, sources_to_depend = _get_object_sources(object_file)

        # Depend on immediate source files:
        redo.redo_ifchange(sources_to_depend)

        # Sort sources by Ada and C/C++
        ada_sources_to_depend = [dep for dep in sources_to_depend if dep.endswith('.ads') or dep.endswith('.adb')]
        c_sources_to_depend = [dep for dep in sources_to_depend if dep.endswith('.h') or dep.endswith('.c') or
                               dep.endswith('.hpp') or dep.endswith('.cpp')]

        # Build all Ada dependencies for this object recursively:
        deps = sources_to_depend
        if ada_sources_to_depend:
            with source_database() as db:
                deps.extend(
                    _build_all_ada_dependencies(
                        ada_sources_to_depend, db
                    )
                )

        # Build all C/C++ dependencies for this object recursively:
        if c_sources_to_depend:
            from database.c_source_database import c_source_database
            with c_source_database() as db:
                deps.extend(
                    _build_all_c_dependencies(
                        c_sources_to_depend, db, build_target_instance
                    )
                )

        # Write deps to file so we can use it for dependency tracking later:
        deps = list(set(deps))
        with open(
            temp_object_dir + os.sep + os.path.basename(object_file) + ".deps", "w"
        ) as f:
            f.write("\n".join([build_target_file, __file__] + deps))

        # Append sources to global lists for bulk compilation:
        # source_files.extend(sources_to_depend)
        sources_to_compile.append(source_to_compile)
        all_deps.extend(deps)

    # Info print if we are compiling a lot of objects, so the user is informed what is going on.
    num_objects = len(object_files)
    if num_objects >= 10:
        redo.info_print(
            "Compiling "
            + str(num_objects)
            + " object"
            + ("s..." if num_objects > 1 else "...")
        )

    # Run gprbuild to compile the sources:
    _run_gprbuild_command(build_target_instance, sources_to_compile, source_dependencies=all_deps, object_dir=temp_object_dir)

    if num_objects >= 10:
        redo.info_print(
            "Moving "
            + str(num_objects)
            + " object"
            + ("s..." if num_objects > 1 else "...")
        )


# Returns true if the object has already been built. In this case the object is moved to
# the final location and dependency tracking has been handled.
def _handle_prebuilt_object(redo_1, redo_2, redo_3):
    # First see if the object has already been compiled and is in the object temp dir. This
    # is a speed optimization that may have already occurred.
    temp_object_dir = os.environ["OBJECT_PRE_BUILD_DIR"]
    temp_object_file = os.path.join(temp_object_dir, os.path.basename(redo_1))
    if os.path.isfile(temp_object_file):
        debug.debug_print("object already built at location " + temp_object_file)
        import glob

        temp_object_glob = temp_object_file[:-1] + "*"
        files_to_copy = glob.glob(temp_object_glob)

        # Move the temporary stuff over to the final location:
        build_dir = os.path.dirname(redo_2)
        filesystem.safe_makedir(build_dir)
        debug.debug_print("mv " + temp_object_file + " " + redo_3)
        move(temp_object_file, redo_3)
        debug.debug_print("mv " + temp_object_glob + " " + build_dir)
        for f in files_to_copy:
            if f != temp_object_file:
                move(f, os.path.join(build_dir, os.path.basename(f)))

        # After the move there should be a dependencies file created from
        # the precompile that we can use to track dependencies. Lets open
        # it and use it:
        with open(build_dir + os.sep + os.path.basename(redo_1) + ".deps", "r") as f:
            deps = f.read().split("\n")
            redo.redo_ifchange(deps)

        # Exit early, we are done, no need to compile...
        return True
    else:
        return False


####################################################
# End speed optimization functions.
####################################################

def _compile_single_object(redo_1, redo_2, redo_3, build_target_instance, source_to_compile, deps):
    # Form temporary files paths:
    build_dir, base_name = redo_arg.split_redo_arg(redo_2)
    temp_dir = os.path.join(build_dir, base_name + "-temp")
    temp_file = os.path.join(temp_dir, base_name + ".o")
    filesystem.safe_makedir(temp_dir)

    # Run gprbuild to compile the sources:
    _run_gprbuild_command(build_target_instance, [source_to_compile], source_dependencies=deps, object_dir=temp_dir)

    # Move the temporary stuff over to the final location:
    debug.debug_print("mv " + temp_file + " " + redo_3)
    move(temp_file, redo_3)
    for f in os.listdir(temp_dir):
        move(os.path.join(temp_dir, f), os.path.join(build_dir, f))
    rmtree(temp_dir)

    # Write a file with the object's full dependencies:
    with open(redo_1 + ".deps", "w") as f:
        f.write("\n".join(deps))


# This function compiles an ada object file from a given
# source file.
def _compile_ada_object(redo_1, redo_2, redo_3, source_files, db):
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

    # Get the build target instance:
    build_target = redo_arg.get_target(redo_2)
    build_target_instance, build_target_file = _get_build_target_instance(build_target)

    # Depend and build the source file:
    deps = source_files + [build_target_file, __file__]
    redo.redo_ifchange(deps)

    # Build all dependencies for these source files (recursively):
    deps += _build_all_ada_dependencies(
        source_files, db
    )
    deps = list(set(deps))

    # Compile:
    _compile_single_object(redo_1, redo_2, redo_3, build_target_instance, source_to_compile, deps)


# This function compiles a c object file from a given
# source file.
def _compile_c_object(redo_1, redo_2, redo_3, source_files, db):
    # GCC wants an c or cpp file if it exists, since we cannot
    # build an object from just a .h file.
    source_to_compile = None
    source_to_compile_extension = None
    for source_file in source_files:
        _, source_to_compile_extension = os.path.splitext(source_file)
        if source_to_compile_extension in [".c", ".cpp", ".S", ".s"]:
            source_to_compile = source_file
            break

    # Make sure we found a source to compile. If we did not find one, alert the
    # user.
    if not source_to_compile:
        error.error_abort("No C/C++ source files were found to build '" + redo_1 + "'.")

    # Get the build target instance:
    build_target = redo_arg.get_target(redo_2)
    build_target_instance, build_target_file = _get_build_target_instance(build_target)

    # Depend and build the source file:
    deps = source_files + [build_target_file, __file__]
    redo.redo_ifchange(deps)

    # Build all dependencies for these source files (recursively):
    deps += _build_all_c_dependencies(
        source_files, db, build_target_instance
    )
    deps = list(set(deps))

    # Compile:
    _compile_single_object(redo_1, redo_2, redo_3, build_target_instance, source_to_compile, deps)


# This build rule is capable of compiling object files from either
# Ada or C/C++ source. It matches on any source file in the system
# and produces objects of the same name as the source file in the
# build/obj directory. If the source file is found in the Ada source
# database, then it is compiled as Ada source. If the file is not found
# in the Ada source database, but is found in the C/C++ source database
# it is compiled as C source.
class build_object(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Optimization: If the object has already been built as part of a prebuild
        # routine along with other objects, then all we need to do is move the
        # object to its final location (redo_3) and then we are done.
        if _handle_prebuilt_object(redo_1, redo_2, redo_3):
            # Exit early, we are done, no need to compile...
            return

        # Make sure the object is being built in the correct directory:
        if not redo_arg.in_build_obj_dir(redo_1):
            error.error_abort(
                "Object file '"
                + redo_1
                + "' can only be built in a 'build/obj/$TARGET' directory."
            )

        # Get the source files associated with this object:
        # First, let's assume this object can be built from Ada
        # source code.
        package_name = ada.file_name_to_package_name(redo_2)
        source_files = None
        with source_database() as db:
            source_files = db.try_get_sources(package_name)

            # Indeed this is Ada source, compile it.
            if source_files:
                return _compile_ada_object(
                    redo_1, redo_2, redo_3, source_files, db
                )

        # If no source files were found, then maybe this object
        # is built from C or C++ source.
        # Try to grab C or C++ source files based on the
        # basename.
        basename = redo_arg.get_base_no_ext(redo_2)
        from database.c_source_database import c_source_database
        import unqlite

        with c_source_database() as db:
            # A lot of the time a c database won't even exist, so we but a try/except
            # block to catch this case and display the appropriate error message.
            try:
                source_files = db.try_get_source(basename)
            except unqlite.UnQLiteError:
                error.error_abort(
                    "No source files were found to build '" + redo_1 + "' (UnQLiteError)."
                )
            except TypeError:
                error.error_abort(
                    "No source files were found to build '" + redo_1 + "' (TypeError)."
                )

            # Indeed this is C source, compile it.
            if source_files:
                return _compile_c_object(
                    redo_1, redo_2, redo_3, source_files, db
                )

        # If we still have not found source files to build this
        # object, then we are out of luck. Alert the user.
        error.error_abort("No Ada or C/C++source files were found to build '" + redo_1 + "'.")

    # Match any Ada source file, as any Ada source file can produce
    # an object. In C/C++ only the .c or .cpp files produce object
    # code, so ignore header files.
    def input_file_regex(self):
        return [r"^((?!template/).)*\.ad[sb]$", r"^((?!template/).)*\.(h|hpp|c|cpp|s)$"]

    # Output object files will always be stored with the same name
    # as their source package, and in the build/obj directory.
    def output_filename(self, input_filename):
        base = redo_arg.get_base_no_ext(input_filename)
        directory = redo_arg.get_src_dir(input_filename)
        return os.path.join(
            directory,
            "build"
            + os.sep
            + "obj"
            + os.sep
            + target.get_default_target()
            + os.sep
            + base
            + ".o",
        )
