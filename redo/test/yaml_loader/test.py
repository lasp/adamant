#!/usr/bin/env python3

# Unit test for the model YAML loader in redo/util/yaml_loader.py: that it
# returns the same data and line numbers as ruamel.yaml's round trip loader
# for the scalars and structures model files use, for less common YAML, and
# for every model file in this repository, that it raises a YAML error for
# YAML it cannot load, and that what it returns survives the model cache.
import datetime
import glob
import os
import pickle
import sys

import ruamel.yaml
from util import yaml_loader

ROOT = os.path.realpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", ".."))

# Values a model file may hold, each loaded as the value of a key.
SCALARS = [
    "text", "two words", "'quoted'", '"double"', "'123'", '"true"', "",
    "true", "True", "TRUE", "false", "False", "FALSE",
    "yes", "no", "on", "off", "Yes", "y", "n",
    "null", "Null", "NULL", "~",
    "0", "1", "-1", "+1", "007", "010", "1_000", "0x1F", "0X1F", "0o17", "0b101", "-0x10",
    "1.5", "-1.5", "+1.5", "1.", ".5", "1e3", "1E3", "1.5e-3", "1_0.5", ".inf", "-.inf", ".Inf", ".nan", ".NaN",
    "1:30", "1.2.3", "0x", "1e", "e3", "+", ".",
    "Interfaces.Unsigned_16", "U16", "Packed_U16.T", "[1, two, 3.0]", "{a: 1, b: two}",
]

# Less common YAML that the round trip loader also loads.
OTHER_YAML = {
    "an anchor and an alias": "a: &x [1, two]\nb: *x\n",
    "a boolean with an anchor": "a: &x true\nb: *x\n",
    "a timestamp": "a: 2020-01-01\n",
    "a merge key": "a: &x {p: 1}\nb:\n  <<: *x\n  q: 2\n",
    "a block string": "a: |\n  line one\n  line two\nb: >\n  folded\n  text\n",
}

# YAML the loader must raise a YAML error for, which a model reports as an
# error in its file.
ERRORS = {
    "a key given twice": "a: 1\na: 2\n",
    "two documents": "a: 1\n---\nb: 2\n",
    "yaml that does not parse": "a: [1, 2\nb: 3\n",
    "a tab used to indent": "a:\n\tb: 1\n",
    "an alias with no anchor": "a: *x\n",
    "a tag of its own": "a: !thing 1\n",
}

NESTED = """---
description: A description.
fields:
  - name: One
    type: Interfaces.Unsigned_16
    list: [1, 2, 3]
  - name: Two

    nested:
      deeper:
        - a
        - b: 1
"""


def check(description, condition):
    if not condition:
        print("FAILED: " + description, file=sys.stderr)
        sys.exit(1)


def ruamel_load(text):
    return ruamel.yaml.YAML(typ="rt").load(text)


def plain(data):
    """The data with every value reduced to its plain python type, to compare the two loaders."""
    if isinstance(data, dict):
        return {plain(key): plain(value) for key, value in data.items()}
    if isinstance(data, list):
        return [plain(value) for value in data]
    if isinstance(data, bool) or type(data).__name__ == "ScalarBoolean":
        return ("bool", bool(data))
    if isinstance(data, int):
        return ("int", int(data))
    if isinstance(data, float):
        return ("float", repr(float(data)))
    if isinstance(data, str):
        return ("str", str(data))
    if isinstance(data, (datetime.date, datetime.datetime)):
        return ("time", str(data))
    return (type(data).__name__, repr(data))


def lines(data, found):
    """The line of every dictionary and list in the data, in order."""
    if isinstance(data, dict):
        found.append(data.lc.line)
        for value in data.values():
            lines(value, found)
    elif isinstance(data, list):
        found.append(data.lc.line)
        for value in data:
            lines(value, found)
    return found


def raises_yaml_error(text):
    try:
        yaml_loader.load(text)
    except ruamel.yaml.YAMLError:
        return True
    return False


def model_files():
    kinds = [os.path.basename(f)[:-len(".yaml")] for f in glob.glob(os.path.join(ROOT, "gen", "schemas", "*.yaml"))]
    files = []
    for directory in ("src", "gen", "doc", "redo"):
        for filename in glob.glob(os.path.join(ROOT, directory, "**", "*.yaml"), recursive=True):
            parts = os.path.basename(filename).split(".")
            if os.sep + "build" + os.sep in filename or len(parts) < 3 or parts[-2] not in kinds:
                continue
            files.append(filename)
    return sorted(files)


def run_cases():
    for scalar in SCALARS:
        text = "key: " + scalar + "\n"
        check("'" + scalar + "' loads as the round trip loader loads it", plain(yaml_loader.load(text)) == plain(ruamel_load(text)))
        if scalar and scalar[0] not in "[{":
            text = scalar + ": value\n"
            check("'" + scalar + "' loads as a key as the round trip loader loads it", plain(yaml_loader.load(text)) == plain(ruamel_load(text)))

    check("an empty file loads as nothing", yaml_loader.load("") is None and ruamel_load("") is None)
    check("a file of only a document start loads as nothing", plain(yaml_loader.load("---\n")) == plain(ruamel_load("---\n")))

    nested = yaml_loader.load(NESTED)
    check("nested data loads as the round trip loader loads it", plain(nested) == plain(ruamel_load(NESTED)))
    check("every dictionary and list has the line the round trip loader gives it", lines(nested, []) == lines(ruamel_load(NESTED), []))
    check("dictionaries keep the order of the file", list(nested["fields"][0].keys()) == ["name", "type", "list"])

    restored = pickle.loads(pickle.dumps(nested, protocol=pickle.HIGHEST_PROTOCOL))
    check("the data survives the model cache", plain(restored) == plain(nested) and lines(restored, []) == lines(nested, []))

    for description, text in OTHER_YAML.items():
        check(description + " loads as the round trip loader loads it", plain(yaml_loader.load(text)) == plain(ruamel_load(text)))

    # The round trip loader keeps a scalar with an explicit tag in an object
    # of its own. This loader gives the value the tag asks for.
    tagged = yaml_loader.load("a: !!str 1\nb: !!int '2'\n")
    check("a scalar with an explicit tag loads as the value the tag asks for", tagged == {"a": "1", "b": 2})

    for description, text in ERRORS.items():
        check(description + " raises a YAML error", raises_yaml_error(text))

    files = model_files()
    check("this repository's model files are found", len(files) > 100)
    loaded = 0
    for filename in files:
        with open(filename, "r") as f:
            text = f.read()
        # A model file with Jinja directives is not YAML until it is rendered.
        if "{{" in text or "{%" in text:
            continue
        loaded += 1
        ours = yaml_loader.load(text)
        theirs = ruamel_load(text)
        check(filename + " loads as the round trip loader loads it", plain(ours) == plain(theirs))
        check(filename + " has the lines the round trip loader gives it", lines(ours, []) == lines(theirs, []))
    check("nearly every model file was compared", loaded > 0.9 * len(files))
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    run_cases()
