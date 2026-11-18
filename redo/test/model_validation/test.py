#!/usr/bin/env python3

# Unit test for schema validation in gen/models/base.py: that a well formed
# model loads, that a model with an unknown key, a missing required key, or a
# value of the wrong type is rejected with an error naming the model file,
# that validation leaves the model's parsed contents as they were read, and
# that a schema is parsed once per process and reused. The fixtures are
# record models written to a temporary directory.
import os
import sys
import tempfile

import database.setup
from models import base
from models import record
from models.exceptions import ModelException

GOOD = """---
description: A record.
fields:
  - name: Value
    description: A value.
    type: Interfaces.Unsigned_16
    format: U16
"""


# Two fields that share one boolean through an anchor and an alias.
ANCHORED = GOOD + """    skip_validation: &flag false
  - name: Other
    description: Another value.
    type: Interfaces.Unsigned_16
    format: U16
    skip_validation: *flag
"""


def check(description, condition):
    if not condition:
        print("FAILED: " + description, file=sys.stderr)
        sys.exit(1)


def load(directory, name, contents):
    filename = os.path.join(directory, name + ".record.yaml")
    with open(filename, "w") as f:
        f.write(contents)
    return record.record(filename)


def rejected(directory, name, contents):
    try:
        load(directory, name, contents)
    except ModelException as e:
        return name + ".record.yaml" in str(e)
    return False


def run_cases(directory):
    good = load(directory, "good", GOOD)
    check("a well formed model loads", len(good.fields) == 1)
    check(
        "validation does not write schema defaults into the model's contents",
        list(good.data["fields"][0].keys()) == ["name", "description", "type", "format"],
    )

    check("an unknown key is rejected", rejected(directory, "unknown_key", GOOD + "bogus: 1\n"))
    check("a missing required key is rejected", rejected(directory, "missing_key", "---\ndescription: A record.\n"))
    check("a value of the wrong type is rejected", rejected(directory, "wrong_type", GOOD.replace("name: Value", "name: [1, 2]")))

    anchored = load(directory, "anchored", ANCHORED)
    check("a boolean given through an anchor or an alias is accepted", len(anchored.fields) == 2)
    check(
        "a boolean with an anchor is rejected where the schema asks for an integer",
        rejected(directory, "anchored_integer", GOOD + "    variable_length_offset: &flag true\n"),
    )

    schema = base._loaded_schemas.get(good.full_schema)
    check("a schema is cached after its first use", schema is not None)
    load(directory, "another", GOOD)
    check("a later model reuses the cached schema", base._loaded_schemas[good.full_schema] is schema)
    check("one schema was parsed for all of the records", len(base._loaded_schemas) == 1)
    check("a model is still rejected against a cached schema", rejected(directory, "unknown_key_again", GOOD + "bogus: 1\n"))
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    # The loads look models up in the session's model database, so the test
    # runs inside one session, established here from this file as the
    # top-level target.
    session = (__file__, os.path.splitext(__file__)[0], __file__ + ".out")
    established = database.setup.setup(*session)
    try:
        with tempfile.TemporaryDirectory() as directory:
            run_cases(directory)
    finally:
        if established:
            database.setup.cleanup(*session)
