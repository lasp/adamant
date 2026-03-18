"""
Persistent target database cache.

After each build, the ephemeral redo_target.db is copied to a persistent
location in /tmp so that 'redo what' can read it instantly without
rebuilding. The cache is keyed by project root path.

Can also be invoked directly to build the full target DB:
    python3 -m database.persistent_target_cache
"""
import hashlib
import os
import os.path
import shutil

from database.database import database, DATABASE_MODE


def _get_project_key():
    """
    Return a stable key identifying the current project environment.

    Uses ADAMANT_CONFIGURATION_YAML env var (unique per project) so the
    same cache is found regardless of which subdirectory you're in.
    Raises if the env var is not set. The adamant environment must be
    activated before using the persistent cache.
    """
    config_yaml = os.environ.get("ADAMANT_CONFIGURATION_YAML")
    if not config_yaml:
        raise EnvironmentError(
            "ADAMANT_CONFIGURATION_YAML is not set. "
            "The adamant environment must be activated (source env/activate) "
            "before using the persistent target cache."
        )
    # Include TARGET in the key so different cross-compile targets
    # (e.g. Linux vs an embedded target) get separate caches.
    target = os.environ.get("TARGET", "")
    return config_yaml + ":" + target


def _get_cache_dir(project_key):
    """Return /tmp/redo-<uid>/ cache directory for the given project."""
    path_hash = hashlib.md5(project_key.encode()).hexdigest()[:12]
    return os.path.join("/tmp", "redo-{uid}".format(uid=os.getuid()), path_hash)


def get_cache_dir():
    """
    Return the cache directory for the current project.

    Also writes a breadcrumb file at /tmp/redo-<uid>/cache_dir so
    shell scripts can find the cache directory without recomputing
    the hash themselves.
    """
    cache_dir = _get_cache_dir(_get_project_key())
    # Write breadcrumb for shell consumption
    try:
        parent = os.path.dirname(cache_dir)
        os.makedirs(parent, exist_ok=True)
        breadcrumb = os.path.join(parent, "cache_dir")
        tmp = breadcrumb + ".tmp"
        with open(tmp, "w") as f:
            f.write(cache_dir + "\n")
        os.replace(tmp, breadcrumb)
    except Exception:
        pass
    return cache_dir


def get_persistent_db_path():
    """
    Return the path to the persistent target DB.
    """
    cache_dir = get_cache_dir()
    return os.path.join(cache_dir, "redo_target.db")


def update_persistent_db_targets(directory, targets):
    """
    Write a directory's targets directly into the persistent DB.
    This is the preferred method when targets are already in memory.
    """
    import unqlite

    persistent_path = get_persistent_db_path()

    try:
        os.makedirs(os.path.dirname(persistent_path), exist_ok=True)
        mode = DATABASE_MODE.READ_WRITE if os.path.isfile(persistent_path) else DATABASE_MODE.CREATE
        with database(persistent_path, mode) as dst:
            dst.store(directory, targets)
    except unqlite.UnQLiteError:
        # DB is corrupt — remove so next build recreates clean.
        try:
            os.remove(persistent_path)
        except OSError:
            pass
    except Exception:
        # Lock contention, permissions, disk full, etc. — leave the
        # existing DB alone; it may still be valid.
        pass


def save_persistent_db(session_db_path):
    """
    Copy the session's redo_target.db to the persistent location.
    Called during build cleanup, before the session dir is destroyed.
    """
    if not os.path.isfile(session_db_path):
        return

    persistent_path = get_persistent_db_path()

    try:
        os.makedirs(os.path.dirname(persistent_path), exist_ok=True)
        # Atomic copy: write to temp file then rename
        tmp_path = persistent_path + ".tmp"
        shutil.copy2(session_db_path, tmp_path)
        os.replace(tmp_path, persistent_path)
    except Exception:
        # Don't let cache save failures break builds
        pass


def generate_text_caches(persistent_db_path=None):
    """
    Generate text cache files for every directory in the persistent DB.

    This gives shell completion instant reads without needing to spawn a
    Python subprocess on the first TAB press for each directory.  Called
    at the end of build_full_target_cache() so that activate pre-warms
    both the persistent DB and the text caches.
    """
    from rules.build_what import save_text_cache, _uniquify_preserve_order
    from rules.build_what_predefined import get_predefined_targets

    if persistent_db_path is None:
        persistent_db_path = get_persistent_db_path()
    if not os.path.isfile(persistent_db_path):
        return

    predefined = get_predefined_targets()

    try:
        # Collect all directories and their targets from the DB.
        # Intermediate dirs like /project/src/components/ won't have
        # DB entries but still need text caches with predefined targets
        # so that tab completion is instant.
        all_dirs = {}  # directory → target list
        with database(persistent_db_path, DATABASE_MODE.READ_ONLY) as db:
            for directory, targets in db.items():
                try:
                    all_dirs[directory] = list(targets)
                except Exception:
                    all_dirs[directory] = []

        # Determine build roots, the top-level directories where we
        # stop generating ancestor text caches.
        from database.setup import get_build_roots
        build_roots = set(os.path.abspath(r) for r in get_build_roots())

        # Add ancestor directories (stop at build roots).
        # For each leaf directory in the DB, walk up the tree and
        # create text caches for intermediate dirs (src/, src/components/,
        # etc.) that have no DB entries.  Skip this when build roots are
        # unknown, since the walk would have no stopping point.
        if build_roots:
            ancestor_dirs = set()
            for directory in list(all_dirs):
                if directory in build_roots:
                    continue
                parent = os.path.dirname(directory)
                while parent and parent != directory:
                    if parent in all_dirs or parent in ancestor_dirs:
                        break
                    ancestor_dirs.add(parent)
                    if parent in build_roots:
                        break
                    directory = parent
                    parent = os.path.dirname(parent)
            for d in ancestor_dirs:
                if d not in all_dirs:
                    all_dirs[d] = []  # predefined targets only

        # Write text cache for each directory
        for directory, targets in all_dirs.items():
            redo_targets = list(predefined)
            if targets:
                targets.sort()
                for target in targets:
                    redo_targets.append(os.path.relpath(target, directory))
                redo_targets = _uniquify_preserve_order(redo_targets)
            save_text_cache(directory, redo_targets)
    except Exception:
        pass


def build_full_target_cache():
    """
    Build the full project target database and save it to the persistent
    cache. This does a complete setup (loading all source/models) so it
    could take a few seconds, but only needs to run once (e.g. from
    env/activate).
    """
    import database.setup

    _get_project_key()  # validates env is set

    # Config yaml is at <project_root>/config/foo.yaml
    config_yaml = os.environ["ADAMANT_CONFIGURATION_YAML"]
    project_root = os.path.dirname(os.path.dirname(config_yaml))

    # Run full setup from project root to populate the ephemeral target DB.
    # These are intentionally fake redo paths used only to satisfy
    # database.setup.setup(redo_1, redo_2, redo_3) path-shaped inputs.
    redo_1 = os.path.join(project_root, "_cache_warmup")
    redo_2 = os.path.join(project_root, "_cache_warmup.tmp")
    redo_3 = os.path.join(project_root, "_cache_warmup.base")

    did_setup = database.setup.setup(redo_1, redo_2, redo_3)
    if did_setup:
        # The setup populated the session target DB, save it
        from database._setup import _get_session_dir
        session_dir = _get_session_dir()
        session_db = os.path.join(session_dir, "db", "redo_target.db")
        save_persistent_db(session_db)
        # Generate text caches for all directories so shell completion
        # is instant from the very first TAB press.
        generate_text_caches()
        # Clean up
        database.setup.cleanup(redo_1, redo_2, redo_3)


# If called directly, this module will build the target cache from scratch.
if __name__ == "__main__":
    build_full_target_cache()
