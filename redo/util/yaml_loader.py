# This module loads the YAML of a model file with ruamel.yaml's safe loader
# instead of its round trip loader. The safe loader runs on libyaml, the C
# YAML parser, when the ruamel.yaml.clib package is installed, which makes it
# much faster. It also returns plain dictionaries and lists, which are smaller
# and faster to store in the model cache than the round trip loader's objects.
#
# The models use one thing from the round trip loader beyond the data: the
# line a dictionary or list started on, for error messages. The safe loader
# does not keep it, so the loader here adds it.


class line_and_column(object):
    """Where a dictionary or list started in the file, counted from zero."""
    __slots__ = ("line", "col")

    def __init__(self, line, col):
        self.line = line
        self.col = col


class yaml_dict(dict):
    """A dictionary loaded from YAML. Its "lc" member says where it started."""
    pass


class yaml_list(list):
    """A list loaded from YAML. Its "lc" member says where it started."""
    pass


# The constructor class is made on the first load. It needs ruamel.yaml, and
# a process that only reads models from the model cache never loads YAML.
_constructor = None


def _get_constructor():
    """Return ruamel.yaml's safe constructor, changed to record where dictionaries and lists start."""
    global _constructor
    if _constructor is None:
        from ruamel.yaml.constructor import SafeConstructor

        class constructor(SafeConstructor):
            def construct_yaml_map(self, node):
                data = yaml_dict()
                data.lc = line_and_column(node.start_mark.line, node.start_mark.column)
                yield data
                data.update(self.construct_mapping(node))

            def construct_yaml_seq(self, node):
                data = yaml_list()
                data.lc = line_and_column(node.start_mark.line, node.start_mark.column)
                yield data
                data.extend(self.construct_sequence(node))

        constructor.add_constructor("tag:yaml.org,2002:map", constructor.construct_yaml_map)
        constructor.add_constructor("tag:yaml.org,2002:seq", constructor.construct_yaml_seq)
        _constructor = constructor
    return _constructor


def load(text):
    """
    Return the data in a string of YAML. It is the data ruamel.yaml's round
    trip loader returns, in dictionaries and lists that have an "lc" member
    holding the line and column they started on.
    """
    import ruamel.yaml

    yml = ruamel.yaml.YAML(typ="safe")
    yml.Constructor = _get_constructor()
    return yml.load(text)
