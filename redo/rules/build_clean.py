import os
import os.path
import secrets
import subprocess
import concurrent.futures
from util import redo
from util import debug
from util import filesystem
from base_classes.build_rule_base import build_rule_base

# Name of the top-level trash directory used by the fast clean. Build
# directories are renamed into here and reclaimed by a detached background
# process. The leading "." ensures the build system's directory walks
# (see util/filesystem.py) never descend into it. The entire directory is
# garbage and is always safe to delete manually.
TRASH_DIR_NAME = ".build_trash"


def _fast_clean_disabled():
    """
    Return True if the user has disabled the fast (asynchronous) clean via
    the DISABLE_FAST_CLEAN environment variable. Unset or "0" means fast
    clean is enabled; any other value disables it, forcing the old
    synchronous delete behavior. Useful for CI or throwaway containers
    where the background reclaimer would be killed before finishing, or
    when disk space must be reclaimed before returning.
    """
    return os.environ.get("DISABLE_FAST_CLEAN", "") not in ("", "0")


class build_clean(build_rule_base):
    """
    This build rule recursively "cleans" from the specified
    directory and below. Cleaning involves removing any
    "build" directories that are found, as well as running
    any "clean.do" files found.

    Removal is asynchronous by default: each build directory is renamed
    (an O(1) metadata operation, fast even on slow virtualized
    filesystems like Docker Desktop bind mounts) into a session
    directory under <repo_root>/.build_trash/, mirroring its original
    path, and a single detached background process reclaims the trash
    with "rm -rf". The tree is logically clean the moment this rule
    returns; the disk space follows shortly after. Set
    DISABLE_FAST_CLEAN=1 to force the old synchronous delete.
    """
    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for clean

    def build(self, redo_1, redo_2, redo_3):
        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all build directories below this directory:
        dirs_to_clean = []
        do_files_to_do = []
        skip_dirs = {"alire"}
        for root, dirnames, files in os.walk(directory):
            dirnames[:] = [d for d in dirnames if not d[0] == "." and not d[0] == "_" and d not in skip_dirs]
            if "build" in dirnames:
                dirs_to_clean.append(os.path.join(root, "build"))
                # We don't want to recurse into build directories
                # so remove them from the list.
                dirnames.remove("build")
            if "clean.do" in files and root != directory:
                do_files_to_do.append(os.path.join(root, "clean"))

        # Remove the build directories:
        if _fast_clean_disabled():
            self._remove_synchronously(dirs_to_clean)
        else:
            self._rename_and_reclaim(directory, dirs_to_clean)

        # Clean any subdirectories:
        if do_files_to_do:
            redo.redo(do_files_to_do)

    @staticmethod
    def _remove_synchronously(dirs_to_clean):
        """
        The old clean behavior: concurrently remove build directories using
        system calls, blocking until every file is deleted. "rm -rf"
        executes much faster than python's rmtree.
        """
        def remove_directory(dir_to_clean):
            debug.debug_print("removing " + dir_to_clean)
            subprocess.run(["rm", "-rf", dir_to_clean], check=True)

        with concurrent.futures.ThreadPoolExecutor() as executor:
            futures = [executor.submit(remove_directory, d) for d in dirs_to_clean]
            for future in concurrent.futures.as_completed(futures):
                try:
                    future.result()
                except subprocess.CalledProcessError as e:
                    debug.debug_print("Error removing directory: " + str(e))

    @staticmethod
    def _rename_and_reclaim(directory, dirs_to_clean):
        """
        The fast clean behavior: rename each build directory into a unique
        session directory under <repo_root>/.build_trash/, mirroring its
        original relative path, then detach a single background process
        that reclaims all trash with "rm -rf". Renames are single
        metadata operations, so this returns in a fraction of the time a
        recursive delete takes on slow filesystems. Any build directory
        that cannot be renamed (e.g. it sits on a different filesystem
        than the trash directory) falls back to a synchronous "rm -rf".

        Trash from previous cleans (e.g. orphaned because the container
        died mid-reclaim) is adopted into this clean's reclaim batch, so
        leftovers never accumulate past the next clean.
        """
        repo_root = filesystem.get_git_root(directory)
        if repo_root is None:
            # Not in a git repository, so there is no sensible stable
            # location for the trash directory. Fall back to the old
            # synchronous behavior.
            build_clean._remove_synchronously(dirs_to_clean)
            return
        trash_root = os.path.join(repo_root, TRASH_DIR_NAME)

        # Rename build directories into a unique session directory. The
        # pid plus random token ensures overlapping or repeated cleans
        # never collide, even across container restarts (pid reuse).
        session_dir = os.path.join(
            trash_root, str(os.getpid()) + "_" + secrets.token_hex(4)
        )
        fallback_dirs = []
        for dir_to_clean in dirs_to_clean:
            rel_path = os.path.relpath(dir_to_clean, repo_root)
            if rel_path.startswith(os.pardir):
                # Outside the repository root; rename destination would
                # escape the trash directory, so just remove it in place.
                fallback_dirs.append(dir_to_clean)
                continue
            # A reclaimer spawned by a previous clean may still be
            # running and can delete our freshly-made parent directories
            # in the window between makedirs and rename (it never touches
            # a renamed build directory itself, since our session name is
            # unique and only enumerated by cleans that start later). One
            # retry wins that race deterministically, because the
            # reclaimer only deletes paths that existed when it was
            # spawned. Persistent failures (e.g. a cross-filesystem
            # rename, EXDEV) fall back to synchronous removal.
            trash_path = os.path.join(session_dir, rel_path)
            for attempt in range(2):
                try:
                    os.makedirs(os.path.dirname(trash_path), exist_ok=True)
                    os.rename(dir_to_clean, trash_path)
                    debug.debug_print("trashing " + dir_to_clean)
                    break
                except OSError:
                    if attempt == 1:
                        fallback_dirs.append(dir_to_clean)
        if fallback_dirs:
            build_clean._remove_synchronously(fallback_dirs)

        # Collect everything currently in the trash: our session directory
        # plus any orphans from previous cleans. A concurrent clean's live
        # session may also be adopted; racing "rm -rf" processes on the
        # same tree are harmless (ENOENT is ignored by -f, and a
        # concurrent rename losing its destination parent falls back to
        # the synchronous path above).
        try:
            trash_paths = [
                os.path.join(trash_root, entry) for entry in os.listdir(trash_root)
            ]
        except OSError:
            trash_paths = []
        if not trash_paths:
            return

        # Detach a single background reclaimer to delete the trash. The
        # new session ensures it survives this process exiting, and the
        # null stdio ensures it holds no pipe to a terminal that may be
        # gone by the time it finishes. We intentionally do not wait on
        # it; the deletion cost moves off the critical path. The
        # reclaimer demotes itself to the lowest CPU and I/O priority
        # (best effort; silently skipped where renice/ionice are
        # unavailable) so a build started immediately after clean takes
        # precedence. After the trash is deleted the trash directory
        # itself is removed if it is empty (rmdir refuses otherwise, so
        # a concurrent clean that is still filling it is left alone; a
        # rename racing the rmdir falls back to the synchronous path
        # above).
        debug.debug_print(
            "reclaiming in background: " + " ".join(trash_paths)
        )
        reclaim_script = (
            "renice -n 19 -p $$ >/dev/null 2>&1;"
            " ionice -c 3 -p $$ 2>/dev/null;"
            ' rm -rf -- "$@";'
            " rmdir -- \"$0\" 2>/dev/null"
        )
        try:
            subprocess.Popen(
                ["/bin/sh", "-c", reclaim_script, trash_root] + trash_paths,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                start_new_session=True,
            )
        except OSError:
            # If the reclaimer cannot even be spawned (no /bin/sh, or the
            # system is out of resources), the build directories have
            # already been renamed away, so the tree is logically clean.
            # Leave the trash for the next clean to adopt rather than
            # surfacing an error: clean is best effort and stays silent.
            debug.debug_print("could not spawn background reclaimer")

    # No need to provide these for "redo clean"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
