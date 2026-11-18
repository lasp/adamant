#!/usr/bin/env python3

# Unit test for model sharing in gen/models/base.py: that a record,
# enumeration, or array model is loaded once per process and the same object
# handed back for every later load of its file, whether the load goes through
# model_loader or constructs the model class directly, that a load asked to
# ignore the cache builds a fresh object, and that a model its class does not
# mark immutable_after_load (a component, which the assembly fills in with
# instance data) is built anew on every load. The fixtures are the
# framework's own models, found through the session's model database.
import os
import sys

import database.setup
from util import model_loader
from models import record


def check(description, condition):
    if not condition:
        print("FAILED: " + description, file=sys.stderr)
        sys.exit(1)


def run_cases():
    record_a = model_loader.try_load_model_by_name("Packed_U16", model_types="record")
    record_b = model_loader.try_load_model_by_name("Packed_U16", model_types="record")
    check("a record loads once and is shared", record_a is not None and record_a is record_b)
    check("the shared record is marked immutable_after_load", type(record_a).immutable_after_load)

    enums_a = model_loader.try_load_model_by_name("Ccsds_Enums", model_types="enums")
    enums_b = model_loader.load_model(enums_a.full_filename)
    check("an enumeration model is shared", enums_a is enums_b)

    array_a = model_loader.try_load_model_by_name("Packed_F32x3", model_types="array")
    array_b = model_loader.try_load_model_by_name("Packed_F32x3", model_types="array")
    check("an array model is shared", array_a is array_b)

    direct = record.record(record_a.full_filename)
    check("constructing the class directly shares too", direct is record_a)

    fresh = model_loader.load_model(record_a.full_filename, ignore_cache=True)
    check("ignoring the cache builds a fresh object", fresh is not record_a)

    component_a = model_loader.try_load_model_by_name("Command_Router", model_types="component")
    component_b = model_loader.try_load_model_by_name("Command_Router", model_types="component")
    check("a component is not marked immutable_after_load", not type(component_a).immutable_after_load)
    check("a component is built anew on every load", component_a is not component_b)

    check("the record's fields are reachable from the shared object", len(record_a.fields) == 1)
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    # The loads look models up in the session's model database, so the test
    # runs inside one session, established here from this file as the
    # top-level target.
    session = (__file__, os.path.splitext(__file__)[0], __file__ + ".out")
    established = database.setup.setup(*session)
    try:
        run_cases()
    finally:
        if established:
            database.setup.cleanup(*session)
