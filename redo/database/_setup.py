import os.path
import git
import random
from util import error
from util import redo_arg
from util import target
from util import filesystem
from util import debug
from collections import OrderedDict
from database.build_target_database import build_target_database
import atexit

# Note about this module:
# This module is private in that it exposes no public
# function that are meant to be called externally. The
# exception is database/setup.py, which calls the _setup()
# function in this module. The purpose of splitting the two
# modules up is to improve performance. Most of the time
# the setup module is used, it does not need to import all the
# many modules that these function require. By splitting the
# files we avoid spending time doing a lot of unnecessary
# module imports.


class _build_path(OrderedDict):
    """
    Extended dictionary object mapping all directories in
    the build path to the filenames found in that directory:

    The dictionary is ordered, since order matters for things like
    c compilation. We want to include project specific paths last
    so that they can overwrite things in the database last, taking
    precedence over same-named things in the core framework.
    ie. src/core/stuff.h in a project specific directory will be
    built/used instead of a src/core/stuff.h in a framework dir
    """
    def __init__(self, search_roots=[], targets=["all"], additional_path=[]):
        """
        Upon init construct the entire build path. This is the only
        public method of the class.
        """
        # If search roots were given then form a build path from each
        # of the roots:
        if search_roots:
            for root in search_roots:
                self._form_build_path(root, targets)

        # If an additional path was given, then add each directory in
        # that path to the object:
        if additional_path:
            for directory in additional_path:
                try:
                    self[directory] = filesystem.get_files_in_dir(directory)
                except FileNotFoundError:
                    self[directory] = []

        # If there is an Adamant configuration yaml defined then we need
        # to add it to the path. # This file is usually not in the path if
        # compiling within Adamant and not the project directory. Adamant
        # NEVER depends on any files within a project directory, except
        # for this one small exception. In this case, we want to include
        # the configuration file in the path since Adamant depends on it.
        if "ADAMANT_CONFIGURATION_YAML" in os.environ:
            config_file = os.environ["ADAMANT_CONFIGURATION_YAML"]
            if config_file:
                config_file = os.path.realpath(config_file)
                config_dir = os.path.dirname(config_file)
                try:
                    self[config_dir] = filesystem.get_files_in_dir(config_dir)
                except FileNotFoundError:
                    self[config_dir] = []

    def _add_to_path(self, directory, filenames):
        """Add a directory + filenames to the object"""
        full_filenames = [
            os.path.join(directory, filename)
            for filename in filenames
            if filename[0] != "."
        ]
        self[directory] = full_filenames

    def _form_build_path(self, search_root, targets=["all"]):
        """
        Compute the build path given a search root and targets. This function
        recursively searches from the search root looking for ".all_path" and
        ".TARGET_path" files. Each directory with a file of that type located in
        it is added to the object.
        """
        def path_filename(target):
            return "." + target + "_path"

        # Otherwise, form the build path by looking for .all_path and
        # .TARGET_path files in the system.
        path_filenames = [path_filename(target) for target in targets]
        # path_filenames = \
        #     [path_filename(target.replace("_Test","")) if target.endswith("_Test") else path_filename(target) \
        #      for target in targets]
        # ^ This old _Test path logic is deprecated. It is now up to the user to define the target correctly to
        # make sure the _Test target includes the regular target path files. This automagic was confusing and now
        # no longer exists.

        for root, dirnames, filenames in filesystem.recurse_through_repo(search_root):
            # sys.stderr.write("root: " + str(root) + " dirnames: " + str(dirnames) + \
            #                  " filenames: " + str(filenames) + "\n")
            # If a filename of .all_path or .target_path is found, then save that directory:
            for path_filename in path_filenames:
                if path_filename in filenames:
                    self._add_to_path(root, filenames)
                    break


def _get_git_root(path):
    """Get the root directory of the adamant repository."""
    try:
        git_repo = git.Repo(path, search_parent_directories=True)
        git_root = git_repo.git.rev_parse("--show-toplevel")
    except BaseException:
        error.error_abort(
            "No valid git repository was found containing the directory: " + str(path)
        )
    return git_root


def _get_path_from_env(var):
    """
    Get a path from an environment variable. Return
    the path as a list of directories.
    """
    try:
        path = os.environ[var]
    except KeyError:
        return []
    return [os.path.realpath(directory) for directory in path.split(":")]


def _get_extra_build_path():
    """Get the extra build path."""
    return _get_path_from_env("EXTRA_BUILD_PATH")


def _get_extra_build_roots():
    """Get the extra build roots."""
    return _get_path_from_env("EXTRA_BUILD_ROOTS")


def _get_user_build_path():
    """
    Get the total build path, as provided by
    the user.
    """
    return _get_path_from_env("BUILD_PATH")


def _get_user_build_roots():
    """
    Get the build roots, as provided by the
    user.
    """
    return _get_path_from_env("BUILD_ROOTS")


def _get_user_remove_build_path():
    """
    Get the remove build path, as provided by the
    user.
    """
    return _get_path_from_env("REMOVE_BUILD_PATH")


def _get_build_roots(cwd):
    """
    Get the build roots from the user, or
    assume some for ourselves if one is not
    given.
    """
    # Return the user build root if it exists.
    user_build_roots = _get_user_build_roots()
    if user_build_roots:
        return user_build_roots
    # The user build root does not exist. Let's
    # assume that the build roots consist of two
    # paths:
    #  1. The adamant root directory, which we can
    #     calculate based on the location of this
    #     module
    #  2. The root of the current git repository that
    #     we are in. This may or may not be the same
    #     as the adamant root directory.
    else:
        # Get the adamant root:
        adamant_root = _get_git_root(os.path.realpath(__file__))
        # Get root of the current git repo:
        current_root = _get_git_root(cwd)
        return list(set([adamant_root, current_root]))


def _sanitize_path(path):
    """
    Sanitize a list of directories, removing empty
    strings and removing duplicates.
    """
    path = list(filter(bool, path))
    path = list(set(path))
    return path


def _get_random_session_id():
    session_id = str(random.randint(0, 999999))
    os.environ["ADAMANT_SESSION_ID"] = session_id
    return session_id


def _get_session_id():
    # Return the session ID if it is already set, otherwise create a
    # session ID to use and save it off.
    #
    # Note, if using a version of redo with the REDO_SESSION variable, it is
    # more optimal to use this as the session number, since there will be one
    # unique number for the entire run of redo. Without the REDO_SESSION variable
    # Adamant will create a new session ID for each top level target, ie.
    # "redo a b c" will create a new session for each a, b, and c. This will
    # cause recreation of databases and caches 3 times, which is slow, but
    # will still work. So use the REDO_SESSION if available.
    try:
        return os.environ["ADAMANT_SESSION_ID"]
    except KeyError:
        try:
            session_id = os.environ["REDO_SESSION"]
            os.environ["ADAMANT_SESSION_ID"] = session_id
            return session_id
        except KeyError:
            return _get_random_session_id()


def _get_session_dir():
    # Set up some temporary directories:
    tempdir = os.environ["ADAMANT_TMP_DIR"]
    return tempdir + os.sep + _get_session_id()


def _get_random_session_dir():
    # Set up some temporary directories:
    tempdir = os.environ["ADAMANT_TMP_DIR"]
    return tempdir + os.sep + _get_random_session_id()


def _setup(redo_1, redo_2, redo_3, sandbox=False):
    """
    Method which sets up the databases for the build system.
    This involves first constructing the current build path
    and then using that path to populate the databases.
    """
    # Set up some temporary directories:
    # If this is a sandbox, we create a new temporary directory
    # with a random name. Otherwise, we use the existing directory
    # and only create one if one does not already exist for this
    # build session.
    if sandbox:
        linkdir = _get_random_session_dir()
    else:
        linkdir = _get_session_dir()
    os.environ["SESSION_TMP_DIR"] = linkdir
    # Create temporary directory to store source code links. This optimization
    # speeds up object compilation times.
    source_link_dir = linkdir + os.sep + "src"
    # Set environment variable so that object compilation can retrieve this location:
    os.environ["SOURCE_LINK_DIR"] = source_link_dir
    filesystem.safe_makedir(source_link_dir)
    # For any top level target that is running this setup we create a .running
    # file in the session temporary directory. This indicates that this rule
    # (or its dependencies) are currently building. This file will be removed
    # when the building of this target is finished. The purpose of this file is
    # to help indicate to the build system when the temporary directory is no
    # longer being used, and thus can be removed, freeing up RAM space on the
    # build machine.
    open(linkdir + os.sep + redo_1.replace(os.sep, "@@") + ".running", "a").close()
    # Create temporary directory to store object links. This optimization
    # speeds up binding times.
    object_link_dir = linkdir + os.sep + "obj"
    # Set environment variable so that object compilation can retrieve this location:
    os.environ["OBJECT_LINK_DIR"] = object_link_dir
    filesystem.safe_makedir(object_link_dir)
    # Create temporary directory to store pre-built object files. This optimization
    # speeds up compilation times by allowing multiple objects to be built at the
    # same time using GPRBuild.
    object_pre_build_dir = linkdir + os.sep + "obj_pre"
    # Set environment variable so that object compilation can retrieve this location:
    os.environ["OBJECT_PRE_BUILD_DIR"] = object_pre_build_dir
    filesystem.safe_makedir(object_pre_build_dir)
    db_dir = linkdir + os.sep + "db"
    filesystem.safe_makedir(db_dir)

    # Some database entities can be created without
    # the build path. Let's create these now. In fact,
    # we may need some of them in order to calculate
    # the build path, below:
    from database.create import create_pre_build_path
    create_pre_build_path()

    # Form the build path:
    # The total path consists of:
    #   1) Every directory that contains a .all_path file
    #   2) Every directory that contains a .$TARGET_path file
    #   3) Every directory in the $EXTRA_BUILD_PATH
    #   4) The current source directory of the redo build target
    # Unless a user build path is provided, in which case we
    # will just use that instead.

    # See if the user already set up a build path for us. If so
    # we don't need to search for a path at all, which means
    # we don't need a build root.
    user_build_path = _get_user_build_path()
    roots = []
    additional_path = []
    run_dir = redo_arg.get_src_dir(redo_2)
    if user_build_path:
        additional_path = user_build_path
    # A build path was not given, instead should construct the path
    # from the build roots.
    else:
        roots = _get_build_roots(run_dir)

    # The total additional path is sum of the user build path, the extra
    # build path and the source directory of the current redo target.
    additional_path = _sanitize_path(
        additional_path + _get_extra_build_path() + [run_dir]
    )

    # The total build roots are the sum of the user build roots (or the
    # assumed build root if that was not available) and the extra build
    # roots:
    roots = _sanitize_path(roots + _get_extra_build_roots())

    # If the build target is defined in the redo target ie. something like:
    #   redo build/obj/Linux/main.elf
    # would define this build as using the "Linux" target regardless of
    # the environment variable. Otherwise, we will just use the environment
    # variable of the default.
    build_target = None
    if redo_arg.in_build_obj_dir(redo_1) or redo_arg.in_build_bin_dir(redo_1):
        build_target = redo_arg.get_target(redo_1)
    if build_target:
        target.set_target(build_target)
    else:
        build_target = target.get_default_target()
    assert build_target

    # Now that we have found the build target, let's try to load it from the
    # build target database. If we load it, we can see if it specifies more
    # path files that we need to include in the path.
    path_files = []
    try:
        instance = None
        with build_target_database() as db:
            instance, filename = db.get_build_target_instance(build_target)
        path_files = instance.path_files()
    except KeyError:
        pass  # Ignore this for now, the user will get an error later for something more specific.

    # Create the build path:
    targets = list(OrderedDict.fromkeys(["all", build_target] + path_files))
    path = _build_path(
        search_roots=roots, targets=targets, additional_path=additional_path
    )

    # Remove any build paths that the user has asked us to remove:
    remove_path = _get_user_remove_build_path()
    for rm_dir in remove_path:
        if rm_dir in path:
            del path[rm_dir]

    # Set environment with computed paths:
    os.environ["COMPUTED_BUILD_ROOTS"] = os.pathsep.join(roots)
    os.environ["COMPUTED_BUILD_PATH"] = os.pathsep.join(path.keys())

    # Print path if in debug mode:
    if os.environ.get("DEBUG"):
        debug.debug_print("--- Session info: ---")
        debug.debug_print("run_directory = " + run_dir)
        debug.debug_print("ADAMANT_SESSION_ID = " + os.environ["ADAMANT_SESSION_ID"])
        debug.debug_print("SESSION_TMP_DIR = " + os.environ["SESSION_TMP_DIR"])
        debug.debug_print("SOURCE_LINK_DIR = " + os.environ["SOURCE_LINK_DIR"])
        debug.debug_print("OBJECT_LINK_DIR = " + os.environ["OBJECT_LINK_DIR"])
        debug.debug_print("--- Important variables: ---")
        debug.debug_print(
            "ADAMANT_CONFIGURATION_YAML = " + os.environ["ADAMANT_CONFIGURATION_YAML"]
        )
        debug.debug_print("--- Build path computation: ---")
        debug.debug_print("Inputs:")
        debug.debug_print("TARGET = " + target.get_default_target())
        debug.debug_print("BUILD_PATH = " + str(_get_user_build_path()))
        debug.debug_print("BUILD_ROOTS = " + str(_get_user_build_roots()))
        debug.debug_print("EXTRA_BUILD_PATH = " + str(_get_extra_build_path()))
        debug.debug_print("EXTRA_BUILD_ROOTS = " + str(_get_extra_build_roots()))
        debug.debug_print("REMOVE_BUILD_PATH = " + str(_get_user_remove_build_path()))
        debug.debug_print("")
        debug.debug_print("Results:")
        debug.debug_print(
            "COMPUTED_BUILD_ROOTS = " + str(_get_path_from_env("COMPUTED_BUILD_ROOTS"))
        )
        debug.debug_print(
            "COMPUTED_BUILD_PATH = " + str(_get_path_from_env("COMPUTED_BUILD_PATH"))
        )

    # Create the database:
    from database.create import create
    create(path)


def _cleanup():
    """Remove temporary directory if it exists."""
    if not os.environ.get("ADAMANT_DISABLE_SESSION_CLEANUP"):
        temp_dir = _get_session_dir()
        if os.path.isdir(temp_dir):
            from shutil import rmtree

            rmtree(temp_dir)


def _delayed_cleanup(redo_1, redo_2, redo_3):
    """
    Remove temporary directory a few seconds after the build system detects that
    no target is still using it. Because this build process may not know of other
    build processes using the same temp directory, we use the delayed_cleanup.sh
    script below, which checks to make sure no process is using the temp
    directory for at least 5 seconds before removing it. Otherwise, we do not clean
    the directory, and expect the process that is still using it to do so. We
    can detect if a process is still using the temp directory by the presence of
    *.running files in the directory.
    """
    if not os.environ.get("ADAMANT_DISABLE_SESSION_CLEANUP"):
        # First remove our .running file to signify to other processes that we are no
        # longer using the temporary directory, and from our perspective it can be cleaned.
        session_dir = _get_session_dir()
        try:
            os.remove(session_dir + os.sep + redo_1.replace(os.sep, "@@") + ".running")
        except Exception:
            return

        # This script checks for the presence of *.running files. If none are found in the directory for
        # 5 seconds, then we can assume that the directory is no longer needed and we remove it.
        # The delayed cleanup is run in the background so that we can return the terminal to the user
        # immediately.
        from util import shell

        shell.try_run_command(
            "sh "
            + os.environ["ADAMANT_DIR"]
            + os.sep
            + "redo"
            + os.sep
            + "util"
            + os.sep
            + "delayed_cleanup.sh "
            + session_dir
            + " >/dev/null 2>/dev/null &"
        )


# Register an exit_handler for top level process
# to clean up temporary directory.
atexit.register(_cleanup)
