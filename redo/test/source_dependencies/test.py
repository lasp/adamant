#!/usr/bin/env python3

# Unit test for the Ada source scanning in redo/util/ada.py: the "with"
# dependency and implicit parent-package extraction in get_source_dependencies,
# and the body-dependency heuristics in should_depend_on_adb. Each fixture
# below locks in the handling of one declaration shape, most of which have
# broken (or silently produced wrong dependencies) at some point.
import os
import sys
import tempfile

from util import ada


def println(strn=""):
    sys.stderr.write(strn + "\n")


def write_temp_source(source):
    handle, path = tempfile.mkstemp(suffix=".ads", text=True)
    with os.fdopen(handle, "w") as f:
        f.write(source)
    return path


def get_deps(source):
    path = write_temp_source(source)
    try:
        return ada.get_source_dependencies(path)
    finally:
        os.unlink(path)


def get_adb(source):
    path = write_temp_source(source)
    try:
        return ada.should_depend_on_adb(path)
    finally:
        os.unlink(path)


# Each entry: (name, source, dependencies that must be found, names that must
# NOT appear as dependencies).
dependency_cases = [
    (
        "multiline aspect declaration yields parent",
        "with Interfaces;\n"
        "package Component.Foo.Implementation\n"
        "   with SPARK_Mode => On\n"
        "is\n"
        "end Component.Foo.Implementation;\n",
        ["Interfaces", "Component.Foo"],
        [],
    ),
    (
        "single-line aspect declaration yields clean parent",
        "package Component.Bar.Implementation with SPARK_Mode => On is\n"
        "end Component.Bar.Implementation;\n",
        ["Component.Bar"],
        [],
    ),
    (
        "private child yields parent",
        "private package A.B.C is\nend A.B.C;\n",
        ["A.B"],
        [],
    ),
    (
        "plain body yields parent",
        "package body A.B is\nend A.B;\n",
        ["A"],
        [],
    ),
    (
        "multi-line with clause keeps trailing units",
        "with Foo,\n     Bar;\npackage P is\nend P;\n",
        ["Foo", "Bar"],
        [],
    ),
    (
        "declaration after a semicolon on the same line still matches",
        "pragma Elaborate_Body; package A.B is\nend A.B;\n",
        ["A"],
        [],
    ),
    (
        "argument-less aspect on a package is not an include",
        "package P\n   with Pure\nis\nend P;\n",
        [],
        ["Pure"],
    ),
    (
        "with Ghost on a declaration is not an include",
        "package P is\n   procedure Foo\n      with Ghost;\nend P;\n",
        [],
        ["Ghost"],
    ),
    (
        "with Always_Terminates is not an include",
        "package P is\n"
        "   function F return Integer\n"
        "      with Always_Terminates;\n"
        "end P;\n",
        [],
        ["Always_Terminates"],
    ),
    (
        "aspect expression does not mangle the parent",
        "package A.B with Abstract_State => (X) is\nend A.B;\n",
        ["A"],
        [],
    ),
    (
        "mixed case declaration yields clean parent",
        "PACKAGE A.B IS\nEND A.B;\n",
        ["A"],
        [],
    ),
    (
        "child generic instantiation yields parent",
        "with G;\npackage A.B is new G (Integer);\n",
        ["G", "A"],
        [],
    ),
    (
        "child renaming yields parent",
        "with C.D;\npackage A.B renames C.D;\n",
        ["C.D", "A"],
        [],
    ),
    (
        "commented-out declaration is ignored",
        "-- package Zz.Yy is\npackage P is\nend P;\n",
        [],
        ["Zz"],
    ),
    (
        "comment following a semicolon is ignored",
        "pragma Pure; -- package Qq.Rr is\npackage P is\nend P;\n",
        [],
        ["Qq"],
    ),
    (
        "generic formal package is not an include",
        "generic\n   with package Q is new R (<>);\npackage G is\nend G;\n",
        [],
        ["Q"],
    ),
]

# Each entry: (name, source, expected result).
adb_cases = [
    (
        "argument-less Inline aspect",
        "package P is\n   procedure Foo\n      with Inline;\nend P;\n",
        True,
    ),
    (
        "Inline aspect without spaces",
        "package P is\n   procedure Foo with Inline=>True;\nend P;\n",
        True,
    ),
    (
        "Inline aspect with spaces",
        "package P is\n   procedure Foo with Inline => True;\nend P;\n",
        True,
    ),
    (
        "one-line generic package header",
        "generic package G is\nend G;\n",
        True,
    ),
    (
        "generic keyword alone ahead of formals",
        "generic\n   type T is private;\npackage G is\nend G;\n",
        True,
    ),
    (
        "pragma Inline",
        "package P is\n   procedure Foo;\n   pragma Inline (Foo);\nend P;\n",
        True,
    ),
    (
        "plain package needs no body dependency",
        "package P is\n   procedure Foo;\nend P;\n",
        False,
    ),
]


if __name__ == "__main__":
    println("testing get_source_dependencies:")
    for name, source, must_have, must_not_have in dependency_cases:
        deps = get_deps(source)
        for dep in must_have:
            assert dep in deps, (
                name + ": expected '" + dep + "' in " + str(deps)
            )
        for dep in must_not_have:
            assert dep not in deps, (
                name + ": did not expect '" + dep + "' in " + str(deps)
            )
        for dep in deps:
            assert dep and " " not in dep, (
                name + ": malformed dependency '" + dep + "' in " + str(deps)
            )
        println("  " + name)

    println("testing should_depend_on_adb:")
    for name, source, expected in adb_cases:
        result = get_adb(source)
        assert result == expected, (
            name + ": expected " + str(expected) + ", got " + str(result)
        )
        println("  " + name)

    println("passed.")
    println()
