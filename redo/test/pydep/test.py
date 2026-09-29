#!/usr/bin/env python3

# Unit test for the dependency scanning in redo/util/pydep.py: a file under
# a build-machinery entry of sys.path, one named redo or gen, is not a
# dependency, so the walk neither reports nor follows it. The fixture is a
# small module tree written to a temporary directory and put on sys.path.
# Building generated modules is exercised by the python tests in gen/test/.
import os
import shutil
import sys
import tempfile

from util import pydep


def write_tree(root, files):
    for relative_path, source in files.items():
        path = os.path.join(root, relative_path)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(source)


fixture = {
    "seed.py": (
        "import helper\n"
        "import build_rule\n"
        "import generator\n"
    ),
    "helper.py": "",
    "redo/build_rule.py": "import only_via_build_rule\n",
    "redo/only_via_build_rule.py": "",
    "gen/generator.py": "",
}


def paths(root, *relative_paths):
    return [os.path.join(root, p) for p in relative_paths]


def check_equal(name, actual, expected):
    assert actual == expected, (
        name + ": expected " + str(expected) + ", got " + str(actual)
    )
    print("  " + name, file=sys.stderr)


def run_cases(root):
    write_tree(root, fixture)
    fixture_path = [root] + paths(root, "redo", "gen")
    sys.path[:0] = fixture_path
    seed = os.path.join(root, "seed.py")

    # seed imports helper and one module from each build tree; build_rule in
    # turn imports a module that only it reaches. helper is the one
    # dependency, at the scanner and after the walk alike.
    print("testing the build-machinery fence:", file=sys.stderr)
    check_equal(
        "the redo and gen entries on sys.path are build machinery",
        [r for r in pydep.build_machinery_roots() if r.startswith(root)],
        paths(root, "redo", "gen"),
    )
    check_equal(
        "a build-machinery file is not a dependency",
        pydep.pydep(seed),
        (paths(root, "helper.py"), []),
    )
    check_equal(
        "the walk neither reports nor follows one",
        pydep._build_pydeps(seed),
        ([], paths(root, "helper.py")),
    )

    print("passed.\n", file=sys.stderr)


if __name__ == "__main__":
    root = tempfile.mkdtemp()
    try:
        run_cases(root)
    finally:
        shutil.rmtree(root)
