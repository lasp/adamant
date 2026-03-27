from database.redo_target_database import redo_target_database
from database.persistent_target_cache import get_persistent_db_path, get_cache_dir, update_persistent_db_targets
from database.database import database, DATABASE_MODE
import os.path
import os
import sys
from base_classes.build_rule_base import build_rule_base
from os import environ
from rules.build_what_predefined import get_predefined_targets


def _uniquify_preserve_order(lst):
    """Remove duplicates while preserving first-occurrence order."""
    return sorted(set(lst), key=lambda x: lst.index(x))


def _dir_to_key(directory):
    """
    Encode a directory path as a filename: /foo/bar → _foo_bar.

    Must match the shell's encoding in redo_completion.sh:
        dir_key="${abs_dir//\\//_}"
    """
    return directory.replace("/", "_")


def save_text_cache(directory, lines):
    """
    Write redo what output lines to the text cache for a directory.

    The text cache stores one target per line as plain text for instant
    shell-side reads with no Python invocation.
    """
    try:
        cache_dir = get_cache_dir()
        dir_key = _dir_to_key(directory)
        text_path = os.path.join(cache_dir, "what", dir_key + ".txt")
        os.makedirs(os.path.dirname(text_path), exist_ok=True)
        tmp_path = text_path + ".tmp"
        with open(tmp_path, "w") as f:
            f.write("\n".join(lines) + "\n")
        os.replace(tmp_path, text_path)
    except Exception:
        pass


class build_what(build_rule_base):
    """
    This build rule lists all the known redo targets that
    can be built in a certain directory on stdout. This
    rule is useful for a user trying to determine what type
    of products can be built in a certain directory.
    """
    def build(self, redo_1, redo_2, redo_3):
        """
        We override build here for performance. With redo what there is no
        need to load the source code and models from the entire project,
        we just need to build path to include the directory pointed to by
        redo_1. So set that in the environment for speed, then call the
        normal implementation of build.

        If a persistent target database exists from a previous build,
        we use it directly, skipping the expensive _setup() entirely.
        The persistent DB is updated automatically by each build.
        """
        directory = os.path.dirname(os.path.abspath(redo_1))
        persistent_db = get_persistent_db_path()
        if persistent_db and os.path.isfile(persistent_db):
            # Fast path: read from persistent DB, no setup needed.
            # Data may be stale but that's fine — next build refreshes.
            try:
                self._build_from_persistent(redo_1, directory, persistent_db)
                return
            except KeyError:
                pass  # Directory not in DB, fall through to slow path
            except Exception:
                pass  # DB corrupt, fall through to slow path
        # No persistent DB or DB corrupt.
        # Setup for just this directory (slower path).
        # Mark that we're running 'redo what' so _delayed_cleanup skips
        # overwriting the persistent DB with our partial single-directory data.
        environ["_REDO_WHAT_ACTIVE"] = "1"
        environ["BUILD_PATH"] = directory + os.pathsep + directory + os.sep + ".."
        self._last_targets = None
        super(build_what, self).build(redo_1, redo_2, redo_3)
        # Merge this directory's targets into persistent cache.
        if self._last_targets is not None:
            try:
                update_persistent_db_targets(directory, self._last_targets)
            except Exception:
                pass

    @staticmethod
    def _output_targets(directory, redo_targets):
        """Write targets to stderr and save text cache for shell completion."""
        unique = _uniquify_preserve_order(redo_targets)
        sys.stderr.write("redo " + "\nredo ".join(unique) + "\n")
        try:
            save_text_cache(directory, unique)
        except Exception:
            pass

    def _build_from_persistent(self, redo_1, directory, persistent_db):
        """Read targets from the persistent DB without running setup.

        Raises RuntimeError if the directory is not in the persistent DB
        at all (KeyError), which signals the caller to fall back to the
        slow path.  An empty target list is a valid cache hit. It means
        the directory exists in the project but has no file-level build
        targets (only predefined ones like all/clean/style).
        """
        redo_targets = get_predefined_targets()
        with database(persistent_db, DATABASE_MODE.READ_ONLY) as db:
            # Let KeyError propagate. That's a real cache miss.
            targets = list(db.fetch(directory))
        if targets:
            targets.sort()
            for target in targets:
                rel_target = os.path.relpath(target, directory)
                redo_targets.append(rel_target)
        self._output_targets(directory, redo_targets)

    def _build(self, redo_1, redo_2, redo_3):
        # Define the special targets that exist everywhere...
        redo_targets = get_predefined_targets()
        directory = os.path.dirname(redo_1)
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except Exception:
                targets = []
        # Save for persistent cache update in build()
        self._last_targets = targets
        if targets:
            targets.sort()
            for target in targets:
                rel_target = os.path.relpath(target, directory)
                redo_targets.append(rel_target)
        self._output_targets(directory, redo_targets)

    # No need to provide these for "redo what"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
