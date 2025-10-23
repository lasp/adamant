import os
import sys
import ast
import importlib.util
import argparse
from util import redo
from database.py_source_database import py_source_database
from base_classes.build_rule_base import build_rule_base
from util import shell


def pydep(source_file, path=[], ignore_list=[]):
    """
    Return dependencies for a given python source file.
    Two lists are returned to the user. The first list is
    the modules found whose source files actually exist on
    the system. The second list includes modules
    that were found in the source file, but could not be
    found on the file system.
    """
    # If a path is not provided than just use the python
    # path variable:
    if not path:
        path = os.environ["PYTHONPATH"].split(":")

    with open(source_file, "r") as f:
        root = ast.parse(f.read())

    existing_deps = []
    nonexistent_deps = []

    def is_system_spec(spec):
        # Must be string, must be a file path, and must not be a system package
        return spec.origin is None or \
               os.sep not in spec.origin or \
               "/usr/lib/python" in spec.origin or \
               "site-packages/" in spec.origin

    for node in ast.walk(root):
        if isinstance(node, ast.Import):
            for alias in node.names:
                name = alias.name  # name of the module
                if any(ignored in name for ignored in ignore_list):
                    continue
                try:
                    spec = importlib.util.find_spec(name)
                except ModuleNotFoundError:
                    nonexistent_deps.append(name)
                    continue
                # if module (and its origin file) exists, append to the existing_deps
                if spec is not None:
                    if is_system_spec(spec):
                        nonexistent_deps.append(name)
                    else:
                        existing_deps.append(spec.origin)
                else:
                    nonexistent_deps.append(name)

        if isinstance(node, ast.ImportFrom):
            name = node.module  # name of the module
            if name:
                if any(ignored in name for ignored in ignore_list):
                    continue
                try:
                    spec = importlib.util.find_spec(name)
                except ModuleNotFoundError:
                    nonexistent_deps.append(name)
                    continue
                if spec is not None:
                    if is_system_spec(spec):
                        nonexistent_deps.append(name)
                    else:
                        existing_deps.append(spec.origin)
                else:
                    nonexistent_deps.append(name)

    return list(set(existing_deps)), nonexistent_deps


def _build_pydeps(source_file, path=[]):
    """
    Recursively build any missing python module dependencies for
    a given source file.
    Collect any static existing dependencies.
    """
    built_deps = []
    all_existing_deps = []
    deps_not_in_path = []

    def _inner_build_pydeps(source_file):
        # Find the python dependencies:
        existing_deps, nonexistent_deps = pydep(source_file, path)
        all_existing_deps.extend(existing_deps)

        # For the nonexistent dependencies, see if we have a rule
        # to build those:
        if nonexistent_deps:
            deps_to_build = []
            with py_source_database() as db:
                deps_to_build = db.try_get_sources(nonexistent_deps)

            deps_not_in_path.extend(deps_to_build)

            # Don't rebuild anything we have already built:
            deps_to_build = [d for d in deps_to_build if d not in built_deps]

            # Build the deps:
            if deps_to_build:
                redo.redo_ifchange(deps_to_build)
                built_deps.extend(deps_to_build)

                # Run py deps on each of the build source files:
                for dep in deps_to_build:
                    _inner_build_pydeps(dep)

    _inner_build_pydeps(source_file)
    return list(set(deps_not_in_path)), list(set(all_existing_deps))


class _build_python_no_update(build_rule_base):
    """
    Class which helps us build the dependencies of a python file using
    the build system.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Build any dependencies:
        return _build_pydeps(redo_1)


class _build_python(build_rule_base):
    """
    Class which helps us build the dependencies of a python file using
    the build system.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Build any dependencies:
        deps_not_in_path, existing_deps = _build_pydeps(redo_1)

        # Figure out what we need to add to the path:
        paths_to_add = list(set([os.path.dirname(d) for d in deps_not_in_path]))

        # Add the paths to the path:
        sys.path.extend(paths_to_add)

        return deps_not_in_path, existing_deps


class _run_python(build_rule_base):
    """
    Class which helps us run a python file using the build system.
    This has the major benefit of building all python dependencies that
    are autogenerated prior to running the actual python file to be executed.
    """
    def _build(self, redo_1, redo_2, redo_3):
        # Build any dependencies:
        deps_not_in_path = _build_pydeps(redo_1)

        # Figure out what we need to add to the path:
        paths_to_add = list(set([os.path.dirname(d) for d in deps_not_in_path]))

        # Run the python script:
        shell.run_command(
            "PYTHONPATH=$PYTHONPATH:" + ":".join(paths_to_add) + " python " + redo_1
        )


def build_py_deps(source_file=None, update_path=True):
    # If the source file is none, then use the source file of this function caller:
    if not source_file:
        import inspect

        frame = inspect.stack()[1]
        module = inspect.getmodule(frame[0])
        source_file = module.__file__
    if update_path:
        rule = _build_python()
    else:
        rule = _build_python_no_update()
    built_deps, existing_deps = rule.build(
        redo_1=source_file,
        redo_2=os.path.splitext(source_file)[0],
        redo_3=source_file + ".out",
    )

    # Reset the database, so that this function can be run again, if warranted.
    import database.setup

    database.setup.reset()

    return built_deps, existing_deps


def run_py(source_file):
    rule = _run_python()
    rule.build(
        redo_1=source_file,
        redo_2=os.path.splitext(source_file)[0],
        redo_3=source_file + ".out",
    )

    # Reset the database, so that this function can be run again, if warranted.
    import database.setup

    database.setup.reset()


# This can also be run from the command line:
if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output"
    )

    parser.add_argument(
        "-p", "--paths",
        action="store_true",
        required=True,
        help="Print resolved dependency paths"
    )

    parser.add_argument(
        "-i", "--ignore",
        nargs="+",
        default=[],
        help="Strings to ignore in dependency names"
    )

    parser.add_argument(
        "file_args",
        nargs="*",
        help="Paths to Python files"
    )

    args = parser.parse_args()

    all_static_deps = set()

    for source_file in args.file_args:
        existing_deps, nonexistent_deps = pydep(source_file, ignore_list=set(args.ignore))

        if args.verbose:
            print(f"\nFinding dependencies for: {source_file}")
            print("\nExisting dependencies:")
            for dep in existing_deps:
                print(dep)

            print("\nNonexistent dependencies:")
            for dep in nonexistent_deps:
                print(dep)

            print("\nBuilding nonexistent dependencies:")

        built_deps, static_existing_deps = build_py_deps(source_file)
        # Collect all existing dependencies:
        all_static_deps.update(built_deps)
        all_static_deps.update(static_existing_deps)

    # print full paths on mode flag
    if args.paths:
        if args.verbose:
            print("\nAll static dependency paths:")

        print("\n".join(sorted(all_static_deps)))
