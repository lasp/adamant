import sys
import os

from util import shell
from util import target

# This modules provides utility functions related to
# finding out meta data about a binary file.


def _get_include_list():
    from database.utility_database import utility_database

    build_path = None
    with utility_database() as db:
        build_path = db.get_source_build_path()
    return build_path


def _get_build_target_instance(target_name):
    from database.build_target_database import build_target_database

    try:
        with build_target_database() as db:
            instance = db.get_build_target_instance(target_name)
    except KeyError:
        return None
    filename = sys.modules[instance.__class__.__module__].__file__
    return instance, filename


def _get_git_root(path):
    import git

    try:
        git_repo = git.Repo(path, search_parent_directories=True)
        return git_repo.git.rev_parse("--show-toplevel")
    except BaseException:
        return None


def _get_git_info(path):
    import git

    try:
        git_repo = git.Repo(path, search_parent_directories=True)
        git_root = git_repo.git.rev_parse("--show-toplevel")
        sha = git_repo.head.object.hexsha
        changed = [item.a_path for item in git_repo.index.diff(None)]
        untracked = git_repo.untracked_files
    except BaseException:
        git_root = "(not in git repo)"
        sha = "(none)"
        changed = "(unknown)"
        untracked = "(unknown)"
    return git_root, sha, changed, untracked


def _safe_get_var(var):
    """
    Get a path from an environment variable. Return
    the path as a list of directories.
    """
    try:
        val = os.environ[var]
    except KeyError:
        return None
    return val


def _run_shell_capture_stdout(command):
    code, stdout, stderr = shell.try_run_command_capture_output(command)
    if stdout:
        return stdout
    else:
        return ""


def _run_shell_capture_stderr(command):
    code, stdout, stderr = shell.try_run_command_capture_output(command)
    if stderr:
        return stderr
    else:
        return ""


def _get_file_type(filename):
    return _run_shell_capture_stdout("file " + filename)[:-1]  # remove \n


def _get_file_hash(filename):
    return _run_shell_capture_stdout("md5sum " + filename).split()[0]


def _print_env(variable):
    val = _safe_get_var(variable)
    string = variable + " = "
    if val:
        string += val
    else:
        string += "(not set)"
    return string


def get_report(
    filename, main_object_filename, redo_temp_filename=None, build_target_instance=None
):
    import datetime
    import os.path

    if redo_temp_filename:
        current_filename = redo_temp_filename
    else:
        current_filename = filename

    assert os.path.isfile(current_filename), (
        "File: '" + current_filename + "' does not exist."
    )

    _string = [""]

    def append_ln(new_string):
        _string[0] += new_string + "\n"

    def print_git_info(name, directory):
        prj_root, prj_sha, prj_changed, prj_untracked = _get_git_info(directory)
        append_ln(name + " git repository: " + prj_root)
        append_ln(name + " git commit: " + prj_sha)
        append_ln(name + " git changed files: " + str(prj_changed))
        append_ln(name + " git untracked files: " + str(prj_untracked))

    # Basic info:
    full_final_filename = os.path.abspath(filename)
    full_main_object_filename = os.path.abspath(main_object_filename)
    full_current_filename = os.path.abspath(current_filename)
    time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    append_ln("Basic Info: ")
    append_ln("Binary meta data for file: " + full_final_filename)
    append_ln("Main object file: " + full_main_object_filename)
    if redo_temp_filename:
        append_ln("Binary meta data for redo temp file: " + full_current_filename)
    append_ln("Report generation time: " + time)
    append_ln(
        "File time stamp: "
        + str(
            datetime.datetime.fromtimestamp(
                os.path.getmtime(full_current_filename)
            ).strftime("%Y-%m-%d %H:%M")
        )
    )
    append_ln("File type: " + _get_file_type(full_current_filename))
    append_ln("File MD5 hash: " + _get_file_hash(full_current_filename))
    append_ln("File size (bytes): " + str(os.path.getsize(full_current_filename)))
    append_ln("")

    # Git info:
    append_ln("Git information: ")
    adamant_dir = _safe_get_var("ADAMANT_DIR")
    if adamant_dir:
        adamant_dir = os.path.realpath(adamant_dir.strip())
        print_git_info("Adamant", adamant_dir)
    prj_dir = _get_git_root(os.path.dirname(full_final_filename))
    if prj_dir:
        prj_dir = os.path.realpath(prj_dir.strip())
        if prj_dir != adamant_dir:
            print_git_info("Project", full_final_filename)
    roots = _safe_get_var("COMPUTED_BUILD_ROOTS")
    if roots:
        roots_list = list(
            set(filter(bool, [a.strip() for a in roots.strip().split(os.pathsep)]))
        )
        for root in roots_list:
            root = os.path.realpath(root)
            if root != adamant_dir and root != prj_dir:
                print_git_info(root.split(os.sep)[-1].title(), root)
    append_ln("")

    # Environment info:
    append_ln("Environment: ")
    append_ln(_print_env("TARGET"))
    append_ln(_print_env("PYTHONPATH"))
    append_ln(_print_env("ADAMANT_DIR"))
    append_ln(_print_env("PATH"))
    append_ln(_print_env("SCHEMAPATH"))
    append_ln(_print_env("TEMPLATEPATH"))
    append_ln(_print_env("BUILD_PATH"))
    append_ln(_print_env("BUILD_ROOTS"))
    append_ln(_print_env("EXTRA_BUILD_PATH"))
    append_ln(_print_env("EXTRA_BUILD_ROOTS"))
    append_ln(_print_env("REMOVE_BUILD_PATH"))
    append_ln(_print_env("COMPUTED_BUILD_ROOTS"))
    append_ln(_print_env("COMPUTED_BUILD_PATH"))
    append_ln("")

    # Build target info:
    if build_target_instance:
        # Hack to determine if this is a gprbuild object or
        # a regular build object:
        gprbuild = False
        try:
            build_target_instance.ada_linker_depends_on()
        except AttributeError:
            gprbuild = True
        append_ln("Build Target: ")
        build_target = target.get_default_target()
        append_ln("Target specified: " + build_target)
        append_ln("Target instance: " + str(build_target_instance))
        append_ln(
            "Target file: "
            + str(sys.modules[build_target_instance.__class__.__module__].__file__)
        )
        append_ln("Target name: " + str(build_target_instance.__class__.__name__))
        if not gprbuild:
            append_ln("Ada compiler: " + str(build_target_instance.ada_compiler()))
            append_ln(
                "Ada compiler flags: " + str(build_target_instance.ada_compiler_flags())
            )
            append_ln(
                "Ada compiler extra includes: "
                + str(build_target_instance.ada_compiler_extra_includes())
            )
            append_ln("Ada binder: " + str(build_target_instance.ada_binder()))
            append_ln(
                "Ada binder extra includes: "
                + str(build_target_instance.ada_binder_extra_includes())
            )
            append_ln(
                "Ada linker: " + str(build_target_instance.ada_binder_extra_includes())
            )
            append_ln(
                "Ada linker flags: " + str(build_target_instance.ada_linker_flags())
            )
            append_ln(
                "Ada linker extra objects: "
                + str(build_target_instance.ada_linker_extra_objects())
            )
            append_ln(
                "Ada linker depends on: "
                + str(build_target_instance.ada_linker_depends_on())
            )
            append_ln("C compiler: " + str(build_target_instance.c_compiler()))
            append_ln(
                "C compiler flags: " + str(build_target_instance.c_compiler_flags())
            )
            append_ln(
                "C compiler extra includes: "
                + str(build_target_instance.c_compiler_extra_includes())
            )
            append_ln(
                "C compiler depends on: "
                + str(build_target_instance.c_compiler_depends_on())
            )
            append_ln("C++ compiler: " + str(build_target_instance.cpp_compiler()))
            append_ln(
                "C++ compiler flags: " + str(build_target_instance.cpp_compiler_flags())
            )
            append_ln(
                "C++ compiler extra includes: "
                + str(build_target_instance.cpp_compiler_extra_includes())
            )
            append_ln(
                "C++ compiler depends on: "
                + str(build_target_instance.cpp_compiler_depends_on())
            )
        append_ln("")

    # GNAT info:
    full_file_dir = os.path.dirname(full_final_filename)
    target_dirname = os.path.basename(full_file_dir)
    basename = "".join(os.path.basename(full_final_filename).split(".")[:-1])
    main_obj = os.path.join(
        full_file_dir,
        ".."
        + os.sep
        + ".."
        + os.sep
        + "obj"
        + os.sep
        + target_dirname
        + os.sep
        + basename
        + ".o",
    )
    if os.path.isfile(main_obj) and build_target_instance:
        append_ln("GNAT Info:")
        if not gprbuild:
            gnat_ls = build_target_instance.gnat_ls()
            if gnat_ls:
                flags = build_target_instance.ada_compiler_flags().strip()
                includes = _get_include_list()
                include_string = "-I" + " -I".join(includes)
                extra_includes = build_target_instance.ada_compiler_extra_includes()
                if extra_includes:
                    include_string += " " + extra_includes.strip()
                cmd = (
                    gnat_ls.strip()
                    + " -a -d -v "
                    + flags
                    + " "
                    + include_string
                    + " "
                    + main_obj
                )
                append_ln(cmd)
                append_ln(_run_shell_capture_stdout(cmd))
                append_ln("")
            else:
                append_ln("gnatls not provided by build target.")

    if "SOURCE_LINK_DIR" in os.environ:
        source_link_dir = os.environ["SOURCE_LINK_DIR"]
        append_ln("Source in build:")
        append_ln("ls -l " + source_link_dir)
        append_ln(_run_shell_capture_stdout("ls -l " + source_link_dir))
        append_ln("")

    if "OBJECT_LINK_DIR" in os.environ:
        object_link_dir = os.environ["OBJECT_LINK_DIR"]
        append_ln("Objects in build:")
        append_ln("ls -l " + object_link_dir)
        append_ln(_run_shell_capture_stdout("ls -l " + object_link_dir))
        append_ln("")

    main_obj_deps_file = full_main_object_filename + ".deps"
    main_obj_deps = None
    if os.path.isfile(main_obj_deps_file):
        append_ln("Dependencies for main object " + full_main_object_filename + ":")
        with open(main_obj_deps_file, "r") as f:
            main_obj_deps = f.read()
            append_ln(main_obj_deps)
        append_ln("")

    if main_obj_deps:
        main_obj_deps.split("\n")

    return _string[0]
