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
        # Clean up
        database.setup.cleanup(redo_1, redo_2, redo_3)


# If called directly, this module will build the target cache from scratch.
if __name__ == "__main__":
    build_full_target_cache()
