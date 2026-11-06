from base_classes.generator_base import generator_base
from generators.basic import basic_generator
from util import model_loader
from util import error
from models import assembly
from models import view
from collections import OrderedDict
import subprocess
import json
import os.path

# This module contains generators which produce a JSON bundle describing an
# assembly, or a view of one, for a viewer: the components, connectors,
# connections, data dependencies and packed types of the model, together with
# the graphviz layout of the same diagram the documentation draws. The bundle
# is the diagram as data; nothing in it is drawn by hand.

SCHEMA_VERSION = 1

# The style the assembly dot template applies, so a viewer draws the same
# diagram the documentation does.
STYLE = OrderedDict(
    [
        ("node_fill", "#F0F5F9"),
        ("name_font", OrderedDict([("face", "helvetica"), ("size", 16)])),
        ("type_font", OrderedDict([("face", "courier"), ("size", 12), ("italic", True)])),
        ("active_penwidth", 3),
        (
            "edge_style",
            OrderedDict(
                [
                    ("recv_async", OrderedDict([("style", "dashed")])),
                    ("service", OrderedDict([("dir", "both"), ("arrowhead", "vee"), ("arrowtail", "onormal")])),
                    ("modify", OrderedDict([("dir", "both"), ("arrowhead", "diamond"), ("arrowtail", "odiamond")])),
                    ("return", OrderedDict([("dir", "both"), ("arrowhead", "inv"), ("arrowtail", "curve")])),
                    ("data_dependency", OrderedDict([("style", "dotted"), ("dir", "both"), ("arrowhead", "curve"), ("arrowtail", "none")])),
                ]
            ),
        ),
    ]
)


def _assembly_ids(a):
    """Map (instance name, suite kind, entity name) to the id the assembly assigned."""
    ids = {}
    for kind in ["data_products", "events", "commands", "parameters", "packets", "faults"]:
        for id, e in getattr(a, kind, {}).items():
            comp = getattr(e, "component", None) or getattr(getattr(e, "suite", None), "component", None)
            if comp is None:
                continue
            ids[(comp.instance_name, kind, e.name)] = id
    return ids


def _ided_entities(suite, kind, instance_name, ids):
    if not suite:
        return []
    return [
        OrderedDict(
            [
                ("name", e.name),
                ("id", ids.get((instance_name, kind, e.name), e.id)),
                ("type", e.type),
                ("description", e.description),
            ]
        )
        for e in suite.entities.values()
    ]


def _connector(conn, invokee_names):
    return OrderedDict(
        [
            ("name", conn.name),
            ("kind", conn.kind),
            ("direction", "invokee" if conn.name in invokee_names else "invoker"),
            ("type", conn.type),
            ("return_type", getattr(conn, "return_type", None)),
            ("count", conn.count),
            ("generic", bool(conn.generic)),
            ("priority", conn.priority),
            ("description", getattr(conn, "description", None)),
        ]
    )


def _component(comp, assembly_dir, ids):
    invokee_names = set(c.name for c in comp.connectors.invokee())
    name = comp.instance_name
    task = None
    if comp.tasks and comp.tasks.has_active_task and comp.tasks.active_task.instantiated:
        task = comp.tasks.active_task.priority
    return OrderedDict(
        [
            ("instance", comp.instance_name),
            ("type", comp.name),
            ("execution", comp.execution),
            ("instance_execution", comp.instance_execution),
            ("priority", task),
            ("has_queue", bool(comp.connectors.of_kind("recv_async"))),
            ("description", comp.description),
            ("model_file", os.path.relpath(comp.full_filename, assembly_dir)),
            ("connectors", [_connector(c, invokee_names) for c in comp.connectors]),
            ("data_products", _ided_entities(comp.data_products, "data_products", name, ids)),
            ("events", _ided_entities(comp.events, "events", name, ids)),
            ("commands", _ided_entities(comp.commands, "commands", name, ids)),
            ("parameters", _ided_entities(comp.parameters, "parameters", name, ids)),
            ("packets", _ided_entities(comp.packets, "packets", name, ids)),
            ("faults", _ided_entities(comp.faults, "faults", name, ids)),
        ]
    )


def _end(component, connector, index):
    return OrderedDict(
        [
            ("component", component.instance_name),
            ("connector", connector.name),
            ("index", index),
        ]
    )


def _connection(c):
    return OrderedDict(
        [
            ("from", _end(c.from_component, c.from_connector, c.from_index)),
            ("to", _end(c.to_component, c.to_connector, c.to_index)),
            ("kind", c.to_connector.kind),
            ("type", c.to_connector.type),
            ("return_type", getattr(c.to_connector, "return_type", None)),
            ("description", c.description),
        ]
    )


def _data_dependencies(a):
    deps = []
    for dd_list in getattr(a, "data_dependencies", {}).values():
        for dd in dd_list:
            deps.append(
                OrderedDict(
                    [
                        ("from", dd.data_product.suite.component.instance_name),
                        ("to", dd.suite.component.instance_name),
                        ("data_product", dd.data_product.name),
                        ("type", dd.type),
                    ]
                )
            )
    return deps


class _types(object):
    """Collects every packed type reachable from the model, by type name."""

    def __init__(self):
        self.types = OrderedDict()

    def add(self, name, model):
        if not name or name in self.types:
            return
        if model is None:
            self.types[name] = self._primitive(name)
            return
        kind = type(model).__name__
        if kind == "record":
            self.types[name] = self._record(model)
        elif kind == "array":
            self.types[name] = self._array(model)
        elif kind == "enum":
            self.types[name] = self._enum(model)
        else:
            self.types[name] = self._primitive(name)

    def _entry(self, name, kind, model):
        return OrderedDict(
            [
                ("name", name),
                ("kind", kind),
                ("size_bits", getattr(model, "size", None)),
                ("description", getattr(model, "description", None)),
                ("variable_length", bool(getattr(model, "variable_length", False))),
                ("fields", []),
                ("element_type", None),
                ("length", None),
                ("literals", []),
            ]
        )

    def _primitive(self, name):
        return self._entry(name, "primitive", None)

    def _field(self, f):
        self.add(f.type, f.type_model if (getattr(f, "is_packed_type", False) or getattr(f, "is_enum", False)) else None)
        return OrderedDict(
            [
                ("name", f.name),
                ("type", f.type),
                ("format", str(f.format) if f.format else None),
                ("start_bit", f.start_bit),
                ("size_bits", f.size),
                ("description", f.description),
                ("is_enum", bool(getattr(f, "is_enum", False))),
            ]
        )

    def _record(self, r):
        entry = self._entry(r.name, "record", r)
        entry["fields"] = [self._field(f) for f in r.fields.values()]
        return entry

    def _array(self, arr):
        entry = self._entry(arr.name, "array", arr)
        el = arr.element
        self.add(el.type, el.type_model if (getattr(el, "is_packed_type", False) or getattr(el, "is_enum", False)) else None)
        entry["element_type"] = el.type
        entry["length"] = arr.length
        return entry

    def _enum(self, en):
        entry = self._entry(en.name, "enum", en)
        entry["literals"] = [
            OrderedDict([("name", lit.name), ("value", lit.value), ("description", lit.description)])
            for lit in en.literals
        ]
        return entry

    def add_component(self, comp):
        for c in comp.connectors:
            self.add(c.type, c.type_model)
            self.add(getattr(c, "return_type", None), getattr(c, "return_type_model", None))
        for suite in [comp.data_products, comp.events, comp.commands, comp.parameters, comp.packets, comp.faults]:
            if suite:
                for e in suite.entities.values():
                    self.add(e.type, e.type_model)


def _layout(a):
    """The graphviz layout of this assembly's own dot diagram, as -Tjson, with the draw
    operations. A diagram with clusters that dot cannot rank is ranked again with newrank."""
    dot_source = a.render("/assembly/name.dot")
    result = subprocess.run(
        ["dot", "-Tjson"], input=dot_source, capture_output=True, text=True
    )
    if result.returncode != 0 and getattr(a, "groups", None):
        result = subprocess.run(
            ["dot", "-Gnewrank=true", "-Tjson"], input=dot_source, capture_output=True, text=True
        )
    if result.returncode != 0:
        error.error_abort("dot failed on the assembly diagram: " + result.stderr)
    return json.loads(result.stdout, object_pairs_hook=OrderedDict)


def build_bundle(a, kind, view_name=None):
    assembly_dir = os.path.dirname(a.full_filename)
    ids = _assembly_ids(a)
    types = _types()
    components = []
    for comp in a.components.values():
        components.append(_component(comp, assembly_dir, ids))
        types.add_component(comp)
    return OrderedDict(
        [
            ("schema_version", SCHEMA_VERSION),
            ("kind", kind),
            ("assembly", a.name),
            ("view", view_name),
            ("description", a.description),
            ("rankdir", getattr(a, "layout", "TB")),
            ("show_switches", OrderedDict(getattr(a, "show_switches", {}))),
            ("style", STYLE),
            ("components", components),
            ("connections", [_connection(c) for c in a.connections]),
            ("data_dependencies", _data_dependencies(a)),
            ("types", types.types),
            ("layout", _layout(a)),
        ]
    )


class assembly_visual_json(basic_generator, generator_base):
    def __init__(self):
        basic_generator.__init__(
            self, model_class=assembly.assembly, template_filename="/assembly/name.visual.json"
        )

    def generate(self, input_filename):
        a = self.model_object(input_filename)
        a.load_view_models()
        a.visual_bundle = build_bundle(a, "assembly")
        a.visual_bundle["views"] = [v.name for v in a.views]
        print(a.render(self.template))


class assembly_view_visual_json(basic_generator, generator_base):
    def __init__(self):
        basic_generator.__init__(
            self, model_class=view.view, template_filename="/assembly/name.visual.json"
        )

    def output_filename(self, input_filename):
        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        build_dir = self._get_default_build_dir()
        output_filename = assembly_name + "_" + view_name + ".visual.json"
        return dirname + os.sep + build_dir + os.sep + output_filename

    def _assembly(self, input_filename):
        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        assembly_model_name = model_loader.get_model_file_path(
            assembly_name, model_types="assembly"
        )
        if not assembly_model_name:
            error.error_print(
                "Could not find a model file for assembly '" + assembly_name + "'."
            )
            error.abort()
        return assembly_model_name, view_name

    def generate(self, input_filename):
        assembly_model_name, view_name = self._assembly(input_filename)
        a = assembly.assembly(assembly_model_name)
        v = view.view(input_filename)
        a = v.apply(a)
        a.visual_bundle = build_bundle(a, "view", view_name)
        print(a.render(self.template))

    def depends_on(self, input_filename):
        assembly_model_name, view_name = self._assembly(input_filename)
        a = assembly.assembly(assembly_model_name)
        return a.get_dependencies() + [assembly_model_name]
