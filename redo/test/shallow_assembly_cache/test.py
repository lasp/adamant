#!/usr/bin/env python3

# Unit test for how the model cache stores a shallow load of an assembly in
# gen/models/assembly.py: that it is built from its file once per session and
# then loaded from its own cache entry, that each load gets its own object,
# that it is never used for a full load, that a load asked to ignore the cache
# or to shallow load only some components is built from file, and that a
# cached full load is preferred. The fixture is the grouped parameters test
# assembly.
import os
import sys

import database.setup
from database.model_cache_database import model_cache_database
from util import model_loader

FIXTURE = os.path.realpath(os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", "..", "..",
    "src", "components", "parameters", "test_grouped", "test_grouped_assembly"))


def check(description, condition):
    if not condition:
        print("FAILED: " + description, file=sys.stderr)
        sys.exit(1)


def cached(model_file):
    with model_cache_database() as db:
        return db.get_model(model_file) is not None


def load(**kwargs):
    return model_loader.try_load_model_by_name("Test_Grouped_Assembly", model_types="assembly", **kwargs)


def run_cases():
    first = load(shallow_load=True)
    check("the assembly model is found", first is not None)
    check("a shallow load is built from its file the first time", not first.from_cache and first.shallow_load)
    check("a shallow load is not saved as the full load", not cached(first.full_filename))

    second = load(shallow_load=True)
    check("a later shallow load comes from the cache", second.from_cache and second.shallow_load)
    check("each shallow load gets its own object", second is not first)
    check("the cached shallow load holds the same components", list(second.components.keys()) == list(first.components.keys()))
    check("the cached shallow load is not saved as the full load", not cached(first.full_filename))

    fresh = load(shallow_load=True, ignore_cache=True)
    check("a shallow load asked to ignore the cache is built from its file", not fresh.from_cache)

    some = load(shallow_load_component_list=[next(iter(first.components.values())).name])
    check("a load that shallow loads only some components is built from its file", not some.from_cache)

    full = load()
    check("a full load is not served from the shallow load", not full.from_cache and not full.shallow_load)
    check("the full load is saved to the cache", cached(full.full_filename))

    third = load(shallow_load=True)
    check("a shallow load prefers the cached full load", third.from_cache and third.submodels == full.submodels and len(third.submodels) > 0)
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    # The cases need a model cache that no earlier build has written to, so they
    # run in a sandbox. The fixture's env.py puts its components on the build
    # path, so the sandbox is created from a target in the fixture's directory.
    target = os.path.join(FIXTURE, "shallow_assembly_cache_test")
    session = (target, target, target + ".out")
    environment = database.setup.create_sandbox(*session)[1]
    try:
        run_cases()
    finally:
        database.setup.destroy_sandbox(*session, prev_env=environment)
