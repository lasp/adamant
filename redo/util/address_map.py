"""
Address map of an Adamant executable for a viewer: every component instance
record with its field layout, the connector handler entry addresses, and the
product database table, all read from the ELF's DWARF and symbol table. The
output is JSON; nothing here depends on the model files.
"""
import hashlib
import json
import re
from collections import OrderedDict

SCHEMA_VERSION = 1

_HANDLER = re.compile(
    r"^component__([a-z0-9_]+)__implementation__([a-z0-9_]+?_(?:recv_sync|recv_async|get|provide|request|modify|return|send))(?:__\d+)?$"
)
# Variables of the generated <assembly>_components package whose type is a component instance record.
_INSTANCE = re.compile(r"^([a-z0-9_]+)_components__([a-z0-9_]+)$")
# A plain component's record is component__<type>__implementation__instance; a generic
# component's instance record is <assembly>_components__<instance>_package__instance.
_COMPONENT_TYPE = re.compile(r"^component__[a-z0-9_]+__implementation__instance$|^[a-z0-9_]+_components__[a-z0-9_]+_package__instance$")

_ENCODING = {1: "address", 2: "bool", 4: "float", 5: "int", 6: "char", 7: "uint", 8: "char"}


def _name(die):
    a = die.attributes.get("DW_AT_name")
    return a.value.decode(errors="replace") if a else None


def _type(die):
    return die.get_DIE_from_attribute("DW_AT_type") if "DW_AT_type" in die.attributes else None


def _strip(die):
    while die is not None and die.tag in ("DW_TAG_typedef", "DW_TAG_const_type", "DW_TAG_volatile_type"):
        die = _type(die)
    return die


def _size(die):
    a = die.attributes.get("DW_AT_byte_size") if die is not None else None
    return a.value if a else None


def _location(die):
    a = die.attributes.get("DW_AT_location")
    if a and isinstance(a.value, list) and a.value and a.value[0] == 3:
        return int.from_bytes(bytes(a.value[1:]), "little")
    return None


def _is_bounds(die):
    """GNAT's bounds record of an unconstrained array: members LB0, UB0, ..."""
    if die.tag != "DW_TAG_structure_type":
        return False
    names = [_name(c) or "" for c in die.iter_children() if c.tag == "DW_TAG_member"]
    return bool(names) and all(re.match(r"^(LB|UB)\d+$", n) for n in names)


def _count(array_die):
    n = 1
    for c in array_die.iter_children():
        if c.tag != "DW_TAG_subrange_type":
            continue
        lo = c.attributes.get("DW_AT_lower_bound")
        hi = c.attributes.get("DW_AT_upper_bound")
        cnt = c.attributes.get("DW_AT_count")
        if cnt and isinstance(cnt.value, int):
            n *= cnt.value
        elif hi and isinstance(hi.value, int):
            n *= hi.value - (lo.value if lo and isinstance(lo.value, int) else 0) + 1
        else:
            return None
    return n


class _Mapper(object):
    def __init__(self, dwarfinfo):
        self.dwarf = dwarfinfo
        self.types = OrderedDict()  # DIE offset -> type entry, for sharing

    def type_entry(self, die, depth=0):
        die = _strip(die)
        if die is None:
            return OrderedDict([("kind", "other")])
        tag = die.tag
        if tag == "DW_TAG_subrange_type" and "DW_AT_type" in die.attributes:
            # GNAT emits a scalar subtype as a subrange over its base type.
            entry = self.type_entry(_type(die), depth)
            lo = die.attributes.get("DW_AT_lower_bound")
            hi = die.attributes.get("DW_AT_upper_bound")
            if lo is not None and isinstance(lo.value, int):
                entry["lower"] = lo.value
            if hi is not None and isinstance(hi.value, int):
                entry["upper"] = hi.value
            if _size(die) is not None:
                entry["size"] = _size(die)
            return entry
        if tag == "DW_TAG_base_type":
            enc = die.attributes.get("DW_AT_encoding")
            return OrderedDict([("kind", _ENCODING.get(enc.value if enc else 0, "other")), ("size", _size(die))])
        if tag == "DW_TAG_enumeration_type":
            literals = OrderedDict()
            for c in die.iter_children():
                if c.tag == "DW_TAG_enumerator":
                    v = c.attributes.get("DW_AT_const_value")
                    literals[str(v.value if v else len(literals))] = _name(c)
            return OrderedDict([("kind", "enum"), ("size", _size(die)), ("literals", literals)])
        if tag in ("DW_TAG_pointer_type", "DW_TAG_reference_type"):
            target = _strip(_type(die))
            entry = OrderedDict([("kind", "pointer"), ("size", _size(die)), ("target", _name(target) if target is not None else None)])
            # A pointer to an array is a table allocated at init; keep the element layout so a reader
            # can walk it, and the bounds record (GNAT names it ...___XUB) so the reader knows its extent.
            if target is not None and depth < 12:
                if target.tag == "DW_TAG_array_type" or _is_bounds(target):
                    entry["target_type"] = self.type_entry(target, depth + 1)
            return entry
        if tag == "DW_TAG_array_type":
            el = self.type_entry(_type(die), depth + 1) if depth < 12 else OrderedDict([("kind", "other")])
            return OrderedDict([("kind", "array"), ("size", _size(die)), ("count", _count(die)), ("element", el)])
        if tag in ("DW_TAG_structure_type", "DW_TAG_union_type", "DW_TAG_class_type"):
            return OrderedDict(
                [
                    ("kind", "record"),
                    ("name", _name(die)),
                    ("size", _size(die)),
                    ("fields", self.fields(die, depth + 1) if depth < 12 else []),
                ]
            )
        if tag == "DW_TAG_subroutine_type":
            return OrderedDict([("kind", "pointer"), ("size", _size(die)), ("target", "subprogram")])
        return OrderedDict([("kind", "other"), ("size", _size(die))])

    def fields(self, die, depth):
        out = []
        for c in die.iter_children():
            if c.tag == "DW_TAG_member":
                out.append(self.member(c, depth, False))
            elif c.tag == "DW_TAG_inheritance":
                parent = _strip(_type(c))
                off = c.attributes.get("DW_AT_data_member_location")
                base = off.value if off else 0
                for f in self.fields(parent, depth):
                    f["offset"] = f["offset"] + base
                    out.append(f)
            elif c.tag == "DW_TAG_variant_part":
                for v in c.iter_children():
                    if v.tag != "DW_TAG_variant":
                        continue
                    for m in v.iter_children():
                        if m.tag == "DW_TAG_member":
                            out.append(self.member(m, depth, True))
        return out

    def member(self, c, depth, variant):
        off = c.attributes.get("DW_AT_data_member_location")
        bit_size = c.attributes.get("DW_AT_bit_size")
        bit_off = c.attributes.get("DW_AT_data_bit_offset")
        entry = OrderedDict(
            [
                ("name", _name(c)),
                ("offset", off.value if off else (bit_off.value // 8 if bit_off else 0)),
                ("type", self.type_entry(_type(c), depth)),
            ]
        )
        if bit_size:
            entry["bit_size"] = bit_size.value
            entry["bit_offset"] = (bit_off.value % 8) if bit_off else 0
        if variant:
            entry["variant"] = True
        return entry


def build_map(elf_path, elffile):
    """Returns the map as an OrderedDict for an open pyelftools ELFFile."""
    dwarf = elffile.get_dwarf_info()
    mapper = _Mapper(dwarf)
    instances = OrderedDict()
    assembly = None
    for cu in dwarf.iter_CUs():
        top = cu.get_top_DIE()
        if "_components" not in (_name(top) or ""):
            continue
        for d in top.iter_children():
            if d.tag != "DW_TAG_variable":
                continue
            m = _INSTANCE.match(_name(d) or "")
            if not m:
                continue
            t = _strip(_type(d))
            # Only the component instance records; the package holds other objects too.
            if t is None or t.tag != "DW_TAG_structure_type" or not _COMPONENT_TYPE.match(_name(t) or ""):
                continue
            assembly = m.group(1)
            instances[m.group(2)] = OrderedDict(
                [
                    ("symbol", _name(d)),
                    ("address", _location(d)),
                    ("size", _size(t)),
                    ("type", _name(t)),
                    ("fields", mapper.fields(t, 0) if t is not None and t.tag == "DW_TAG_structure_type" else []),
                ]
            )
    handlers = OrderedDict()
    symtab = elffile.get_section_by_name(".symtab")
    if symtab is not None:
        for sym in symtab.iter_symbols():
            m = _HANDLER.match(sym.name)
            if m and sym["st_info"]["type"] == "STT_FUNC":
                handlers.setdefault(m.group(1), OrderedDict())[m.group(2)] = sym["st_value"]
    with open(elf_path, "rb") as f:
        digest = hashlib.sha256(f.read()).hexdigest()
    return OrderedDict(
        [
            ("schema_version", SCHEMA_VERSION),
            ("elf", elf_path.split("/")[-1]),
            ("sha256", digest),
            ("assembly", assembly),
            ("instances", instances),
            ("handlers", handlers),
        ]
    )


def write_map(elf_path, out_path):
    from elftools.elf.elffile import ELFFile

    with open(elf_path, "rb") as f:
        m = build_map(elf_path, ELFFile(f))
    with open(out_path, "w") as out:
        json.dump(m, out, indent=1)
    return m
