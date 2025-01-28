import os

def update_env_var(variable_name, paths, overwrite=False):
    """
    Update the specified environment variable with given paths.

    Args:
        variable_name (str): The name of the environment variable.
        paths (str or list of str): A single path or a list of paths to set or append.
        overwrite (bool): If True, overwrite the existing variable; otherwise, append.
    """
    if isinstance(paths, str):
        paths = [paths]

    # Current paths from the environment variable
    current_paths = os.environ.get(variable_name, "").split(os.pathsep)

    if overwrite:
        # Overwrite mode: Only use new paths
        all_paths = filter(None, paths)
    else:
        # Append mode: Combine current and new paths
        all_paths = filter(None, current_paths + paths)

    # Ensure paths are unique
    unique_paths = list(set(all_paths))

    # Set the environment variable with paths joined by os.pathsep
    os.environ[variable_name] = os.pathsep.join(unique_paths)


# Specific functions for each environment variable
def add_to_build_path(paths):
    update_env_var("EXTRA_BUILD_PATH", paths)

def set_build_path(paths):
    update_env_var("BUILD_PATH", paths, overwrite=True)

def set_build_roots(paths):
    update_env_var("BUILD_ROOTS", paths, overwrite=True)

def add_to_build_roots(paths):
    update_env_var("EXTRA_BUILD_ROOTS", paths)

def remove_from_build_path(paths):
    update_env_var("REMOVE_BUILD_PATH", paths)
