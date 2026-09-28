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


def _build_pydeps(seeds, ignore_list=()):
    """
    Walk the imports of the seed files together, building every generated
    module the walk reaches, and return {source file: generated} for each
    file the seeds transitively import, in discovery order. One visited set
    serves all seeds, so a file reached from several seeds is parsed once.
    """
    seeds = [os.path.abspath(seed) for seed in seeds]
    seen = set(seeds)
    dependencies = {}
    built = set()
    to_scan = list(seeds)

    while to_scan:
        resolved = []
        unresolved = []
        for source_file in to_scan:
            files, missing = pydep(source_file, ignore_list)
            resolved.extend(files)
            unresolved.extend(missing)

        generated = []
        wanted = list(dict.fromkeys(unresolved))
        if wanted:
            with py_source_database() as db:
                generated = [os.path.abspath(g) for g in db.try_get_sources(wanted)]
            to_build = [g for g in generated if g not in built]
            if to_build:
                redo.redo_ifchange(to_build)
                built.update(to_build)

        to_scan = []
        for path in generated + [os.path.abspath(f) for f in resolved]:
            if path not in seen:
                seen.add(path)
                to_scan.append(path)
            dependencies[path] = path in built

    return dependencies


def _directories_of(files):
    return list(dict.fromkeys(os.path.dirname(f) for f in files))


class _build_python(build_rule_base):
    """
    Build rule which builds the python dependencies of a set of seed files
    within a build system session and, when asked, puts the directories of
    the built modules on sys.path so the caller can import them.
    """
    def __init__(self, seeds=(), update_path=True, ignore_list=()):
        self.seeds = seeds
        self.update_path = update_path
        self.ignore_list = ignore_list

    def _build(self, redo_1, redo_2, redo_3):
        dependencies = _build_pydeps(self.seeds, self.ignore_list)
        built_deps = [path for path, generated in dependencies.items() if generated]
        existing_deps = [path for path, generated in dependencies.items() if not generated]
        if self.update_path:
            sys.path.extend(_directories_of(built_deps))
        return built_deps, existing_deps


class _run_python(build_rule_base):
    """
    Build rule which runs a python file with its generated dependencies built
    and on the python path.
    """
    def _build(self, redo_1, redo_2, redo_3):
        dependencies = _build_pydeps([redo_1])
        built_deps = [path for path, generated in dependencies.items() if generated]
        shell.run_command(
            "PYTHONPATH=$PYTHONPATH:" + ":".join(_directories_of(built_deps)) + " python " + redo_1
        )


def build_py_deps(source_file=None, update_path=True, ignore_list=()):
    """
    Build the generated python modules that a source file, or a list of
    source files, transitively imports, and return two lists: the built
    modules and the existing source files the imports resolve to. Without a
    source file, the caller's own module is used. The build session is
    established from the first seed. With update_path, the
    directories of the built modules are appended to sys.path so the caller
    can import them. Names in ignore_list, and their submodules, are not
    followed.
    """
    # If the source file is none, then use the source file of this function caller:
    if not source_file:
        import inspect

        frame = inspect.stack()[1]
        module = inspect.getmodule(frame[0])
        source_file = module.__file__
    seeds = [source_file] if isinstance(source_file, str) else list(source_file)
    rule = _build_python(seeds, update_path, ignore_list)
    built_deps, existing_deps = rule.build(
        redo_1=seeds[0],
        redo_2=os.path.splitext(seeds[0])[0],
        redo_3=seeds[0] + ".out",
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
        action="append",
        default=[],
        help="Module name to leave unresolved and unfollowed, along with its submodules; repeatable"
    )

    parser.add_argument(
        "file_args",
        nargs="+",
        help="Paths to Python files"
    )

    args = parser.parse_args()

    if args.verbose:
        for source_file in args.file_args:
            existing_deps, unresolved = pydep(source_file, args.ignore)
            print(f"\nFinding dependencies for: {source_file}")
            print("\nExisting dependencies:")
            for dep in existing_deps:
                print(dep)

            print("\nUnresolved dependencies:")
            for dep in unresolved:
                print(dep)

        print("\nBuilding unresolved dependencies:")

    built_deps, existing_deps = build_py_deps(args.file_args, ignore_list=args.ignore)

    # print full paths on mode flag
    if args.paths:
        if args.verbose:
            print("\nAll dependency paths:")

        print("\n".join(sorted(built_deps + existing_deps)))
