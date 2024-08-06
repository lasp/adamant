import sys
from os import environ

# Global profiler, used if PROFILE_ADAMANT_BUILD environment
# variable is set.
profiler = None


def optimize_path():
    """
    This module provides some performance enhancement to .do files:
    """
    def _optimize_path():
        # Reorder sys.path for optimal import times:
        system_paths = []
        env_paths = []
        other_paths = []
        for p in sys.path:
            if not p.startswith("/home"):
                system_paths.append(p)
            elif ".py_env" in p:
                env_paths.append(p)
            else:
                other_paths.append(p)

        # New path, with preference on system and environment paths:
        path = system_paths + env_paths + other_paths
        str_path = ":".join(path)
        if "OPTIMIZED_PYTHON_PATH" not in environ:
            environ["OPTIMIZED_PYTHON_PATH"] = str_path
        sys.path = path

    # Just calculate the first time, otherwise copy path
    # from environment variable:
    if "OPTIMIZED_PYTHON_PATH" in environ:
        sys.path = environ["OPTIMIZED_PYTHON_PATH"].split(":")
    else:
        _optimize_path()

    # Turn on the build system profiler if we are in that mode:
    if "PROFILE_ADAMANT_BUILD" in environ:
        # Start profiling
        import cProfile
        global profiler
        profiler = cProfile.Profile()
        profiler.enable()


def exit(redo_2):
    """
    Python exit without garbage collection. This is a small performance
    enhancement.
    """
    # Turn off the build system profiler if we are in that mode:
    if "PROFILE_ADAMANT_BUILD" in environ:
        # Stop profiling
        profiler.disable()
        import pstats
        with open(redo_2 + ".profile.txt", "w") as f:
            ps = pstats.Stats(profiler, stream=f).sort_stats('cumtime')
            ps.print_stats()

    from os import _exit

    sys.stdout.flush()
    sys.stderr.flush()
    _exit(0)
