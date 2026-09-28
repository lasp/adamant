"""
Find the python source files a script depends on, and build the ones the
build system generates before the script needs them.

A script that imports generated modules calls build_py_deps on itself: the
imports that resolve to existing files are its static dependencies, and the
ones that do not are looked up in the build database and built. The build
system and the code generators are never dependencies. A script imports
them to run the build, not because its logic needs them, so the walk
neither follows nor reports a file under a sys.path entry named redo or
gen, which is where every build tree in the workspace lives.
"""
import os
import sys
import sysconfig
import ast
import argparse
from importlib.machinery import PathFinder
from util import redo
from database.py_source_database import py_source_database
from base_classes.build_rule_base import build_rule_base
from util import shell


def imported_names(source_file):
    """
    Return the module names a python source file imports, in source order.
    `from X import Y` yields both X and X.Y, since Y is either an attribute
    of X or the submodule X.Y and only the dotted form distinguishes them.
    A star import yields only X. Relative imports are left out.
    """
    with open(source_file, "r") as f:
        root = ast.parse(f.read(), filename=source_file)

    names = []
    for node in ast.walk(root):
        if isinstance(node, ast.Import):
            names.extend(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and node.level == 0:
            names.append(node.module)
            names.extend(node.module + "." + alias.name for alias in node.names if alias.name != "*")
    return list(dict.fromkeys(names))


def locate_module(name):
    """
    Return the files that an import of a dotted name loads, in load order,
    found on sys.path without importing anything. A regular package
    contributes its __init__.py and a module contributes itself; a
    namespace package contributes nothing. The search stops at the first
    module, since anything after it is an attribute, not a submodule.
    Returns None when a component of the name does not exist.
    """
    files = []
    path = None
    parts = name.split(".")
    for depth in range(1, len(parts) + 1):
        spec = PathFinder.find_spec(".".join(parts[:depth]), path)
        if spec is None:
            return None
        if spec.submodule_search_locations is None:
            files.append(spec.origin)
            return files
        if spec.has_location:
            files.append(spec.origin)
        path = list(spec.submodule_search_locations)
    return files


def build_machinery_roots():
    """The sys.path entries that hold the build system and code generators."""
    entries = [os.path.abspath(entry) for entry in sys.path]
    return [entry for entry in entries if os.path.basename(entry) in ("redo", "gen")]


def _under(path, roots):
    return any(path == root or path.startswith(root + os.sep) for root in roots)


_stdlib_roots = [sysconfig.get_paths()[key] for key in ("stdlib", "platstdlib")]


def _installed(path):
    """A file of the standard library or of an installed package."""
    path = os.path.abspath(path)
    return _under(path, _stdlib_roots) or "site-packages" in path or "dist-packages" in path


def pydep(source_file, ignore_list=()):
    """
    Return the dependencies of a python source file as two lists: the
    project files its imports load, and the imported names that load no
    project file, which the build database may know how to generate. An
    import that resolves only to the standard library or to an installed
    package is reported in the second list rather than dropped, since the
    project may generate a module of the same name and the name alone
    cannot tell. Imports of the build machinery and of names in ignore_list
    (or their submodules) appear in neither list.
    """
    files = []
    unresolved = []
    machinery = build_machinery_roots()
    for name in imported_names(source_file):
        if any(name == i or name.startswith(i + ".") for i in ignore_list):
            continue
        loaded = locate_module(name)
        if loaded is None or any(_installed(f) for f in loaded):
            unresolved.append(name)
        elif not any(_under(os.path.abspath(f), machinery) for f in loaded):
            files.extend(loaded)
    return list(dict.fromkeys(files)), list(dict.fromkeys(unresolved))


def _build_pydeps(source_file, path=[]):
    """
    Recursively build any missing python module dependencies for
    a given source file.
    Collect any static existing dependencies.
    Track processed files to guard against circular imports.
    """
    built_deps = []
    all_existing_deps = []
    deps_not_in_path = []
    processed_files = set()

    def _inner_build_pydeps(source_file):
        # Skip if already processed:
        if source_file in processed_files:
            return
        processed_files.add(source_file)
        # Find the python dependencies:
        existing_deps, nonexistent_deps = pydep(source_file)
        all_existing_deps.extend(existing_deps)

        # Collect dependencies to recurse on
        deps_to_recurse = list(existing_deps)

        # For the nonexistent dependencies, see if we have a rule
        # to build those:
        deps_to_build = []
        if nonexistent_deps:
            with py_source_database() as db:
                deps_to_build = db.try_get_sources(nonexistent_deps)

            deps_not_in_path.extend(deps_to_build)

            # Don't rebuild anything we have already built:
            deps_to_build = [d for d in deps_to_build if d not in built_deps]

            # Build the deps:
            if deps_to_build:
                redo.redo_ifchange(deps_to_build)
                built_deps.extend(deps_to_build)
                deps_to_recurse.extend(deps_to_build)

        # Recurse on all dependencies to collect their transitive deps
        for dep in deps_to_recurse:
            _inner_build_pydeps(dep)

    _inner_build_pydeps(source_file)
    return list(dict.fromkeys(deps_not_in_path)), list(dict.fromkeys(all_existing_deps))


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
        paths_to_add = list(dict.fromkeys([os.path.dirname(d) for d in deps_not_in_path]))

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
        paths_to_add = list(dict.fromkeys([os.path.dirname(d) for d in deps_not_in_path]))

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
        existing_deps, nonexistent_deps = pydep(source_file, args.ignore)

        if args.verbose:
            print(f"\nFinding dependencies for: {source_file}")
            print("\nExisting dependencies:")
            for dep in existing_deps:
                print(dep)

            print("\nUnresolved dependencies:")
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
