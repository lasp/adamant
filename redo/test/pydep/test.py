#!/usr/bin/env python3

# Unit test for the python import scanning in redo/util/pydep.py: which
# names a file imports, which files an import loads, what is a dependency,
# what is left to the build database (standard library, installed
# packages) and what is left out (ignored names, build machinery), the
# shared walk over several seed files, and the build of a generated
# module. The fixture is a small module tree written to a temporary
# directory and put on sys.path, plus the record model beside this test,
# whose python module the session's build database knows how to generate.
import importlib
import os
import shutil
import subprocess
import sys
import tempfile

import database.setup
from util import pydep


def write_tree(root, files):
    for relative_path, source in files.items():
        path = os.path.join(root, relative_path)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(source)


fixture = {
    "seed_a.py": (
        "import json\n"
        "from runtime_only.api import call\n"
        "from pkg import mod\n"
        "import ns.leaf\n"
        "from missing_generated import Thing\n"
        "from . import sibling\n"
        "import installed_package\n"
        "from ns import *\n"
    ),
    "seed_b.py": (
        "from pkg import mod\n"
        "from ns import other\n"
    ),
    "pkg/__init__.py": "import os\n",
    "pkg/mod.py": "from ns import leaf\n",
    "ns/leaf.py": "",
    "ns/other.py": "import build_tool\n",
    "tools/build_tool.py": "import ns.leaf\n",
    "site-packages/installed_package.py": "",
    "seed_d.py": "import statistics\n",
    "statistics.py": "",
    "seed_c.py": (
        "import helper\n"
        "import build_rule\n"
        "import generator\n"
    ),
    "helper.py": "",
    "redo/build_rule.py": "import only_via_build_rule\n",
    "redo/only_via_build_rule.py": "",
    "gen/generator.py": "",
    "seed_e.py": "import generated_record\n",
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
    fixture_path = [root] + paths(root, "tools", "site-packages", "redo", "gen")
    sys.path[:0] = fixture_path
    seed_a, seed_b, seed_c, seed_d, seed_e = paths(
        root, "seed_a.py", "seed_b.py", "seed_c.py", "seed_d.py", "seed_e.py"
    )

    # seed_a holds one import of every form. A from-import is reported as
    # both the module and the dotted submodule, a relative import is left
    # out, and a star import contributes only its module.
    print("testing imported_names:", file=sys.stderr)
    check_equal(
        "every import form, from-imports as both package and submodule, relative imports skipped",
        pydep.imported_names(seed_a),
        [
            "json",
            "runtime_only.api", "runtime_only.api.call",
            "pkg", "pkg.mod",
            "ns.leaf",
            "missing_generated", "missing_generated.Thing",
            "installed_package",
            "ns",
        ],
    )

    # pkg is a regular package and ns a namespace package, so a name under
    # pkg loads pkg/__init__.py first while a name under ns loads only the
    # module itself. A name that runs past a module is an attribute of it.
    print("testing locate_module:", file=sys.stderr)
    check_equal(
        "regular package chain includes the package __init__",
        pydep.locate_module("pkg.mod"),
        paths(root, "pkg/__init__.py", "pkg/mod.py"),
    )
    check_equal(
        "namespace package contributes no file",
        pydep.locate_module("ns.leaf"),
        paths(root, "ns/leaf.py"),
    )
    check_equal(
        "name ending at a namespace package loads nothing",
        pydep.locate_module("ns"),
        [],
    )
    check_equal(
        "attribute of a module stops at the module",
        pydep.locate_module("ns.leaf.Leaf"),
        paths(root, "ns/leaf.py"),
    )
    check_equal(
        "missing submodule is None",
        pydep.locate_module("ns.nothing"),
        None,
    )
    check_equal(
        "missing top-level name is None",
        pydep.locate_module("nowhere"),
        None,
    )

    # json is standard library and installed_package lives under a
    # site-packages directory, so neither is a dependency, yet both are
    # reported for the build database alongside missing_generated, which
    # exists nowhere on sys.path, since the project may generate a module
    # of the same name. runtime_only is dropped only while it is ignored.
    # statistics is a standard-library name too, but the fixture's own
    # statistics.py comes first on sys.path, and what a name resolves to
    # decides, not the name.
    print("testing pydep:", file=sys.stderr)
    check_equal(
        "project files as dependencies, everything else left to the build database",
        pydep.pydep(seed_a, ignore_list=["runtime_only"]),
        (
            paths(root, "pkg/__init__.py", "pkg/mod.py", "ns/leaf.py"),
            ["json", "missing_generated", "missing_generated.Thing", "installed_package"],
        ),
    )
    check_equal(
        "an ignored name and its submodules are left unresolved",
        pydep.pydep(seed_a)[1],
        [
            "json", "runtime_only.api", "runtime_only.api.call",
            "missing_generated", "missing_generated.Thing", "installed_package",
        ],
    )
    check_equal(
        "a project file shadowing a standard-library name is a dependency",
        pydep.pydep(seed_d),
        (paths(root, "statistics.py"), []),
    )

    # seed_c imports helper and one module from each build tree; build_rule in
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
        pydep.pydep(seed_c),
        (paths(root, "helper.py"), []),
    )
    check_equal(
        "the walk neither reports nor follows one",
        pydep._build_pydeps([seed_c]),
        {os.path.join(root, "helper.py"): False},
    )

    # seed_a and seed_b both import pkg.mod, and ns.other reaches
    # tools/build_tool.py through a second sys.path entry. One walk over
    # both seeds returns the union of what they reach.
    print("testing the shared walk:", file=sys.stderr)
    check_equal(
        "one walk over two seeds reaches the union of their imports",
        pydep._build_pydeps([seed_a, seed_b], ignore_list=["runtime_only"]),
        {
            path: False
            for path in paths(
                root, "pkg/__init__.py", "pkg/mod.py", "ns/leaf.py", "ns/other.py", "tools/build_tool.py"
            )
        },
    )

    # generated_record is the record model beside this test, so its python
    # module is in the session's build database and nowhere on sys.path.
    # The walk asks the database, builds the module, reports it as
    # generated whether or not an earlier run left it built, and follows
    # its imports: the packed type base class is the one project file.
    print("testing generated modules:", file=sys.stderr)
    this_dir = os.path.dirname(os.path.realpath(__file__))
    adamant = os.path.dirname(os.path.dirname(os.path.dirname(this_dir)))
    generated = os.path.join(this_dir, "build", "py", "generated_record.py")
    base_class = os.path.join(adamant, "gnd", "base_classes", "packed_type_base.py")
    check_equal(
        "an unbuilt generated module is left to the build database",
        pydep.pydep(seed_e),
        ([], ["generated_record"]),
    )
    check_equal(
        "the walk builds it, reports it as generated, and follows its imports",
        pydep._build_pydeps([seed_e]),
        {generated: True, base_class: False},
    )
    check_equal("the built module exists", os.path.isfile(generated), True)

    # The command line walks the same seeds through build_py_deps and prints
    # every dependency path, sorted, one per line.
    print("testing the command line:", file=sys.stderr)
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(fixture_path + [env.get("PYTHONPATH", "")])
    output = subprocess.run(
        [
            sys.executable, pydep.__file__, seed_a, seed_b,
            "-p", "-i", "runtime_only",
        ],
        check=True, capture_output=True, text=True, env=env,
    ).stdout
    check_equal(
        "paths are printed sorted",
        output.splitlines(),
        sorted(paths(root, "pkg/__init__.py", "pkg/mod.py", "ns/leaf.py", "ns/other.py", "tools/build_tool.py")),
    )

    # seed_e adds the one generated module, which comes back in the built
    # list with its directory put on sys.path, so the record imports.
    print("testing build_py_deps:", file=sys.stderr)
    check_equal(
        "a list of seeds yields the built and existing lists",
        pydep.build_py_deps([seed_a, seed_b, seed_e], ignore_list=["runtime_only"]),
        (
            [generated],
            paths(root, "pkg/__init__.py", "pkg/mod.py", "ns/leaf.py", "ns/other.py")
            + [base_class]
            + paths(root, "tools/build_tool.py"),
        ),
    )
    check_equal(
        "the built module is importable",
        importlib.import_module("generated_record").__file__,
        generated,
    )

    print("passed.\n", file=sys.stderr)


if __name__ == "__main__":
    # The walk looks unresolved names up in the session's python source
    # database, so the test runs inside one session, established here from
    # this file as the top-level target.
    session = (__file__, os.path.splitext(__file__)[0], __file__ + ".out")
    established = database.setup.setup(*session)
    root = tempfile.mkdtemp()
    try:
        run_cases(root)
    finally:
        shutil.rmtree(root)
        if established:
            database.setup.cleanup(*session)
