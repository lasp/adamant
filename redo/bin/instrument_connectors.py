#!/usr/bin/env python3
"""
Write a counted copy of Adamant's connector directory: every connector
instance gets a Calls counter that its Call increments, so a memory reader
sees per-connection activity without hooks.

    instrument_connectors.py <adamant_dir> <out_dir>

The copy replaces the framework's connector directory for one build only:

    REMOVE_BUILD_PATH=<adamant_dir>/src/core/connector EXTRA_BUILD_PATH=<out_dir> \\
        redo build/bin/<instrumented target>/main.elf

Nothing in the framework checkout changes. The output carries no path file,
so a default build never sees it.
"""
import os
import re
import sys

CONNECTORS = ("in_connector", "in_out_connector", "in_return_connector", "return_connector")

FIELD_ANCHOR = "      Connected : Boolean := False;\n   end record;"
FIELD_NEW = "      Connected : Boolean := False;\n      Calls : Interfaces.Unsigned_32 := 0;\n   end record;"

CALL_BODY = re.compile(
    r"(\n   (?:function|procedure) Call \(Self : in Instance[^)]*\)(?: return \w+)? is\n)"
    r"(   begin\n      pragma Assert \(Self\.Connected\);\n)"
)
CALL_NEW = (
    r"\1"
    "      use type Interfaces.Unsigned_32;\n"
    "      pragma Warnings (Off, \"*modified via address clause*\");\n"
    "      Calls : Interfaces.Unsigned_32\n"
    "         with Import, Volatile, Address => Self.Calls'Address;\n"
    r"\2"
    "      Calls := Calls + 1;\n"
    "      pragma Warnings (On, \"*modified via address clause*\");\n"
)


def instrument_spec(text, name):
    if FIELD_ANCHOR not in text:
        raise SystemExit("%s.ads: instance record does not end with Connected" % name)
    text = text.replace(FIELD_ANCHOR, FIELD_NEW, 1)
    return "with Interfaces;\n" + text


def instrument_body(text, name):
    text, n = CALL_BODY.subn(CALL_NEW, text)
    if n != 1:
        raise SystemExit("%s.adb: expected one Call body, found %d" % (name, n))
    return text


def main():
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        return 2
    source = os.path.join(sys.argv[1], "src", "core", "connector")
    out = sys.argv[2]
    os.makedirs(out, exist_ok=True)
    for entry in sorted(os.listdir(source)):
        if not entry.endswith((".ads", ".adb")):
            continue
        with open(os.path.join(source, entry)) as f:
            text = f.read()
        name, ext = os.path.splitext(entry)
        if name in CONNECTORS:
            text = instrument_spec(text, name) if ext == ".ads" else instrument_body(text, name)
        with open(os.path.join(out, entry), "w") as f:
            f.write(text)
    sys.stderr.write("instrument_connectors: %s -> %s\n" % (source, out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
