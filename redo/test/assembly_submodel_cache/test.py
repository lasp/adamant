#!/usr/bin/env python3

# Unit test for how the model cache stores an assembly submodel: it has no
# entry of its own, it is built from its file until its assembly is cached,
# and after that it loads from the cached assembly, already resolved. The
# fixture is the grouped parameters test assembly and its parameter table.
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


def run_cases():
    table_file = model_loader.get_model_file_path("Test_Grouped_Assembly", model_types="parameter_table")
    check("the parameter table model is found", table_file is not None)

    alone = model_loader.load_model(table_file)
    check("a submodel is built from its file until its assembly is cached", not alone.from_cache and alone.assembly is None)
    check("a submodel built on its own is not saved to the cache", not cached(table_file))

    assembly = model_loader.try_load_model_by_name("Test_Grouped_Assembly", model_types="assembly")
    check("the assembly holds the submodel", assembly is not None and table_file in assembly.submodels)
    check("the assembly is saved to the cache", cached(assembly.full_filename))
    check("loading the assembly does not save the submodel on its own", not cached(table_file))

    table = model_loader.load_model(table_file)
    check("a submodel loads from its cached assembly", table.from_cache)
    check("that submodel knows its assembly", table.assembly is not None and table.assembly.full_filename == assembly.full_filename)
    check("that submodel is the one its assembly holds", table is table.assembly.submodels[table_file])
    check("that submodel depends on its assembly", assembly.full_filename in table.get_dependencies())

    resolved = table.assembly
    table.load_assembly()
    check("load_assembly keeps the assembly a cached submodel came with", table.assembly is resolved)
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    # The cases need a model cache that no earlier build has written to, so they
    # run in a sandbox and without the persistent model cache. The fixture's
    # env.py puts its components on the build path, so the sandbox is created
    # from a target in the fixture's directory.
    os.environ.pop("ENABLE_PERSISTENT_MODEL_CACHE", None)
    target = os.path.join(FIXTURE, "assembly_submodel_cache_test")
    session = (target, target, target + ".out")
    environment = database.setup.create_sandbox(*session)[1]
    try:
        run_cases()
    finally:
        database.setup.destroy_sandbox(*session, prev_env=environment)
