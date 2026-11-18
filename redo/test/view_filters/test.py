#!/usr/bin/env python3

# Unit test for view filters in gen/models/view.py: that applying a filter, the
# not (~), group (@), and (&) and or (|) combinations, or a whole view leaves the
# assembly it was applied to unchanged, and gives the same result as applying it
# to a freshly loaded assembly. Filters work on a shallow copy of the assembly,
# which is only safe while every filter builds new collections and never edits
# the ones it is given or the components and connections they hold. Every entry
# of filter_type_dict needs a case here. The fixture is the science assembly in
# doc/example_architecture and its views.
import copy
import os
import sys
import types

import database.setup
from models import view
from util import model_loader

FIXTURE = os.path.realpath(os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", "..", "..", "doc", "example_architecture"))


def check(description, condition):
    if not condition:
        print("FAILED: " + description, file=sys.stderr)
        sys.exit(1)


def load(**kwargs):
    return model_loader.try_load_model_by_name("Science_Assembly", model_types="assembly", **kwargs)


def members(obj):
    """Each attribute of an object, by the identity of its value."""
    return sorted((name, id(value)) for name, value in vars(obj).items())


def snapshot(assm):
    """What filtering must leave as it was: the assembly's data, its collections and what they hold."""
    return {
        "data": copy.deepcopy(assm.data),
        "show_switches": dict(assm.show_switches),
        "collections": (id(assm.components), id(assm.connections), id(assm.data_dependencies)),
        "components": [(name, id(c), members(c), copy.deepcopy(c.instance_data)) for name, c in assm.components.items()],
        "connections": [(id(c), members(c), copy.deepcopy(c.data)) for c in assm.connections],
        "data_dependencies": [(key, [(id(dd), members(dd)) for dd in dd_list]) for key, dd_list in assm.data_dependencies.items()],
    }


def describe(assm):
    """What a filter result shows: its components, connections and data dependencies, by name."""
    return (
        [c.instance_name for c in assm.components.values()],
        [(c.from_name, c.to_name) for c in assm.connections],
        sorted((str(key), [(dd.suite.component.instance_name, dd.name) for dd in dd_list]) for key, dd_list in assm.data_dependencies.items()),
    )


def filters(assm):
    """One include and one exclude filter of each type, naming items the assembly has."""
    component = next(iter(assm.components.values()))
    connection = assm.connections[0]
    dependency = next(dd for dd_list in assm.data_dependencies.values() for dd in dd_list)
    items = {
        "component_name": [component.instance_name],
        "component_type": [component.name],
        "component_execution": ["active"],
        "component_name_context": [component.instance_name],
        "component_type_context": [component.name],
        "connector_name": [connection.to_name],
        "connector_type": [connection.to_connector.type or connection.to_connector.return_type],
        "connector_kind": ["send"],
        "data_dependency_name": [dependency.suite.component.instance_name + "." + dependency.name],
        "data_dependency_type": [dependency.type],
    }
    return [view.Filter("%s_%s" % (kind, list_type), kind, item_list, list_type)
            for kind, item_list in items.items() for list_type in ("include", "exclude")]


def operations(assm):
    """Each filter alone and in each combination, as functions of the assembly they filter."""
    def operand(filt):
        return types.SimpleNamespace(apply=filt.create_apply_function(assm))

    out = []
    fs = filters(assm)
    for i, a in enumerate(fs):
        b = fs[(i + 1) % len(fs)]
        out.append(("filter " + a.name, operand(a).apply))
        out.append(("~ " + a.name, view.assembly_not(operand(a))))
        out.append(("@ " + a.name, view.assembly_group(operand(a), types.SimpleNamespace(groups=[]))))
        out.append((a.name + " & " + b.name, view.assembly_intersection(operand(a), operand(b))))
        out.append((a.name + " | " + b.name, view.assembly_union(operand(a), operand(b))))
    return out


def run_cases():
    assm = load()
    check("the assembly model is found", assm is not None)
    check("the fixture has data dependencies", len(assm.data_dependencies) > 0)
    before = snapshot(assm)

    exercised = {(f.type, f.list_type == "include") for f in filters(assm)}
    check("every entry of filter_type_dict has a case: missing " + str(set(view.filter_type_dict) - exercised),
          exercised == set(view.filter_type_dict))

    fresh_operations = dict(operations(load(ignore_cache=True)))
    for name, operation in operations(assm):
        result = view.prune(operation(view._copy_to_filter(assm)))
        check("'" + name + "' leaves the assembly unchanged", snapshot(assm) == before)
        fresh = load(ignore_cache=True)
        expected = view.prune(fresh_operations[name](view._copy_to_filter(fresh)))
        check("'" + name + "' gives the result it gives on a freshly loaded assembly", describe(result) == describe(expected))

    views = model_loader.try_load_models_by_name("Science_Assembly", model_types="view", ignore_cache=True)
    check("the fixture has views", len(views) > 0)
    for v in views:
        result = v.apply(assm)
        check("view '" + v.name + "' leaves the assembly unchanged", snapshot(assm) == before)
        again = {x.name: x for x in model_loader.try_load_models_by_name("Science_Assembly", model_types="view", ignore_cache=True)}[v.name]
        expected = again.apply(load(ignore_cache=True))
        check("view '" + v.name + "' gives the result it gives on a freshly loaded assembly", describe(result) == describe(expected))
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    # The fixture's env.py puts its components on the build path, so the
    # session is a sandbox created from a target in the fixture's directory.
    os.environ.pop("ENABLE_PERSISTENT_MODEL_CACHE", None)
    target = os.path.join(FIXTURE, "view_filters_test")
    session = (target, target, target + ".out")
    environment = database.setup.create_sandbox(*session)[1]
    try:
        run_cases()
    finally:
        database.setup.destroy_sandbox(*session, prev_env=environment)
