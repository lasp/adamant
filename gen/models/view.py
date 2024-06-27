import models.base
from util import ada
import models.assembly
import os
import copy
from collections import OrderedDict
from models.exceptions import ModelException
import sys
import re

#################################################
# Validation functions:
##################################################

MODEL_FILE = "*.view.yaml"


def warn(message):
    sys.stderr.write(
        "View model validation warning for " + MODEL_FILE + ": " + message + "\n"
    )


def get_component_names(assm):
    return {x.instance_name for x in assm.components.values()}


def check_component_names(filter_obj, assm):
    component_names = get_component_names(assm)
    for t in filter_obj.item_list:
        if t not in component_names:
            warn(
                "In filter '"
                + filter_obj.name
                + "' component name '"
                + str(t)
                + "' does not exist in assembly."
            )


def get_component_types(assm):
    return {x.name for x in assm.components.values()}


def check_component_types(filter_obj, assm):
    component_types = get_component_types(assm)
    for t in filter_obj.item_list:
        if t not in component_types:
            warn(
                "In filter '"
                + filter_obj.name
                + "' component type '"
                + str(t)
                + "' does not exist in assembly."
            )


def get_connector_names(assm):
    names = [x.to_name for x in assm.connections]
    return names + [x.from_name for x in assm.connections]


def check_connector_names(filter_obj, assm):
    connector_names = get_connector_names(assm)
    # Include non-indexed names in the list too. ie. if Tick_Divider_Instance.Tick_T_Send[3]
    # is in the list also include Tick_Divider_Instance.Tick_T_Send
    connector_names_no_indexes = [re.sub(r'\[[^\]]*\]$', '', x) for x in connector_names if x.endswith(']')]
    connector_names = list(set(connector_names + connector_names_no_indexes))
    for t in filter_obj.item_list:
        if t not in connector_names:
            warn(
                "In filter '"
                + filter_obj.name
                + "' connector name '"
                + str(t)
                + "' does not exist in assembly."
            )


def get_connector_types(assm):
    return {
        x.to_connector.type if x.to_connector.type else x.to_connector.return_type
        for x in assm.connections
    }


def check_connector_types(filter_obj, assm):
    connector_types = get_connector_types(assm)
    for t in filter_obj.item_list:
        if t not in connector_types:
            warn(
                "In filter '"
                + filter_obj.name
                + "' connector type '"
                + str(t)
                + "' does not exist in assembly."
            )


#################################################
# Filter combinators:
##################################################


def include_component_name_filter(filter_obj, assm):
    check_component_names(filter_obj, assm)

    def f(assm_to_filter):
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.instance_name in filter_obj.item_list:
                components[component.instance_name] = component
        assm_to_filter.components = components
        return assm_to_filter

    return f


def exclude_component_name_filter(filter_obj, assm):
    check_component_names(filter_obj, assm)

    def f(assm_to_filter):
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.instance_name in filter_obj.item_list:
                pass
            else:
                components[component.instance_name] = component
        assm_to_filter.components = components
        return assm_to_filter

    return f


def include_component_type_filter(filter_obj, assm):
    check_component_types(filter_obj, assm)

    def f(assm_to_filter):
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.name in filter_obj.item_list:
                components[component.instance_name] = component
        assm_to_filter.components = components
        return assm_to_filter

    return f


def exclude_component_type_filter(filter_obj, assm):
    check_component_types(filter_obj, assm)

    def f(assm_to_filter):
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.name in filter_obj.item_list:
                pass
            else:
                components[component.instance_name] = component
        assm_to_filter.components = components
        return assm_to_filter

    return f


def include_component_execution_filter(filter_obj, assm):
    def f(assm_to_filter):
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.instance_execution in filter_obj.item_list:
                components[component.instance_name] = component
        assm_to_filter.components = components
        return assm_to_filter

    return f


def exclude_component_execution_filter(filter_obj, assm):
    def f(assm_to_filter):
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.instance_execution in filter_obj.item_list:
                pass
            else:
                components[component.instance_name] = component
        assm_to_filter.components = components
        return assm_to_filter

    return f


def include_component_name_context_filter(filter_obj, assm):
    check_component_names(filter_obj, assm)

    def f(assm_to_filter):
        connections = []
        components = []
        for connection in assm_to_filter.connections:
            if (
                connection.to_component.instance_name in filter_obj.item_list
                or connection.from_component.instance_name in filter_obj.item_list
            ):
                connections.append(connection)
                components.append(connection.to_component.instance_name)
                components.append(connection.from_component.instance_name)
        components = set(components)
        assm_to_filter.connections = connections
        new_components = OrderedDict()
        for c_name, c in assm_to_filter.components.items():
            if c_name in components:
                new_components[c_name] = c
        assm_to_filter.components = new_components
        return assm_to_filter

    return f


def exclude_component_name_context_filter(filter_obj, assm):
    check_component_names(filter_obj, assm)

    def f(assm_to_filter):
        connections = []
        components = []
        for connection in assm_to_filter.connections:
            if (
                connection.to_component.instance_name in filter_obj.item_list
                or connection.from_component.instance_name in filter_obj.item_list
            ):
                pass
            else:
                connections.append(connection)
                components.append(connection.to_component.instance_name)
                components.append(connection.from_component.instance_name)
        components = set(components)
        assm_to_filter.connections = connections
        new_components = OrderedDict()
        for c_name, c in assm_to_filter.components.items():
            if c_name in components:
                new_components[c_name] = c
        assm_to_filter.components = new_components
        return assm_to_filter

    return f


def include_component_type_context_filter(filter_obj, assm):
    check_component_types(filter_obj, assm)

    def f(assm_to_filter):
        comp_to_type = {
            x.instance_name: x.name for x in assm_to_filter.components.values()
        }
        connections = []
        components = []
        for connection in assm_to_filter.connections:
            if (
                comp_to_type[connection.to_component.instance_name]
                in filter_obj.item_list
                or comp_to_type[connection.from_component.instance_name]
                in filter_obj.item_list
            ):
                connections.append(connection)
                components.append(connection.to_component.instance_name)
                components.append(connection.from_component.instance_name)
        components = set(components)
        assm_to_filter.connections = connections
        new_components = OrderedDict()
        for c_name, c in assm_to_filter.components.items():
            if c_name in components:
                new_components[c_name] = c
        assm_to_filter.components = new_components
        return assm_to_filter

    return f


def exclude_component_type_context_filter(filter_obj, assm):
    check_component_types(filter_obj, assm)

    def f(assm_to_filter):
        comp_to_type = {
            x.instance_name: x.name for x in assm_to_filter.components.values()
        }
        connections = []
        components = []
        for connection in assm_to_filter.connections:
            if (
                comp_to_type[connection.to_component.instance_name]
                in filter_obj.item_list
                or comp_to_type[connection.from_component.instance_name]
                in filter_obj.item_list
            ):
                pass
            else:
                connections.append(connection)
                components.append(connection.to_component.instance_name)
                components.append(connection.from_component.instance_name)
        components = set(components)
        assm_to_filter.connections = connections
        new_components = OrderedDict()
        for c_name, c in assm_to_filter.components.items():
            if c_name in components:
                new_components[c_name] = c
        assm_to_filter.components = new_components
        return assm_to_filter

    return f


def include_connector_name_filter(filter_obj, assm):
    check_connector_names(filter_obj, assm)

    def f(assm_to_filter):
        connections = []
        for connection in assm_to_filter.connections:
            if connection.to_name in filter_obj.item_list:
                connections.append(connection)
            elif connection.from_name in filter_obj.item_list:
                connections.append(connection)
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def exclude_connector_name_filter(filter_obj, assm):
    check_connector_names(filter_obj, assm)

    def f(assm_to_filter):
        connections = []
        for connection in assm_to_filter.connections:
            # Currently if you remove an arrayed connector, the following code will remove all
            # components of that array. TODO: maybe make a way to remove one at a time, and another
            # method to remove all of the array.
            if connection.to_name.split("[")[0] in filter_obj.item_list:
                pass
            elif connection.from_name.split("[")[0] in filter_obj.item_list:
                pass
            else:
                connections.append(connection)
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def include_connector_type_filter(filter_obj, assm):
    check_connector_types(filter_obj, assm)

    # Filter function:
    def f(assm_to_filter):
        connections = []
        for connection in assm_to_filter.connections:
            if (
                connection.to_connector.type
                and connection.to_connector.type in filter_obj.item_list
            ):
                connections.append(connection)
            elif (
                connection.to_connector.return_type
                and connection.to_connector.return_type in filter_obj.item_list
            ):
                connections.append(connection)
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def exclude_connector_type_filter(filter_obj, assm):
    check_connector_types(filter_obj, assm)

    def f(assm_to_filter):
        connections = []
        for connection in assm_to_filter.connections:
            if (
                connection.to_connector.type
                and connection.to_connector.type in filter_obj.item_list
            ):
                pass
            elif (
                connection.to_connector.return_type
                and connection.to_connector.return_type in filter_obj.item_list
            ):
                pass
            else:
                connections.append(connection)
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def include_connector_kind_filter(filter_obj, assm):
    def f(assm_to_filter):
        connections = []
        for connection in assm_to_filter.connections:
            if connection.to_connector.kind in filter_obj.item_list:
                connections.append(connection)
            elif connection.from_connector.kind in filter_obj.item_list:
                connections.append(connection)
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def exclude_connector_kind_filter(filter_obj, assm):
    def f(assm_to_filter):
        connections = []
        for connection in assm_to_filter.connections:
            if connection.to_connector.kind in filter_obj.item_list:
                pass
            elif connection.from_connector.kind in filter_obj.item_list:
                pass
            else:
                connections.append(connection)
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


# this dictionary is key'd by the tuple:
#   (filter_type, include) : filter_combinator
# where include is a boolean where:
#   include = True, exclude = False
# the dictionary values are the filter combinator
# which returns a function that takes assembly data
# and returns modified (filtered) assembly data.
filter_type_dict = {
    ("component_name", True): include_component_name_filter,
    ("component_name", False): exclude_component_name_filter,
    ("component_type", True): include_component_type_filter,
    ("component_type", False): exclude_component_type_filter,
    ("component_execution", True): include_component_execution_filter,
    ("component_execution", False): exclude_component_execution_filter,
    ("component_name_context", True): include_component_name_context_filter,
    ("component_name_context", False): exclude_component_name_context_filter,
    ("component_type_context", True): include_component_type_context_filter,
    ("component_type_context", False): exclude_component_type_context_filter,
    ("connector_name", True): include_connector_name_filter,
    ("connector_name", False): exclude_connector_name_filter,
    ("connector_type", True): include_connector_type_filter,
    ("connector_type", False): exclude_connector_type_filter,
    ("connector_kind", True): include_connector_kind_filter,
    ("connector_kind", False): exclude_connector_kind_filter,
}


def prune(assm_to_filter):
    component_names = [x.instance_name for x in assm_to_filter.components.values()]

    # Cleanup 1: Remove connections for which there is no component:
    components_with_connections = []
    new_connections = []
    for connection in assm_to_filter.connections:
        to_component = connection.to_component.instance_name
        from_component = connection.from_component.instance_name
        if (to_component in component_names) and (from_component in component_names):
            new_connections.append(connection)
            components_with_connections.append(to_component)
            components_with_connections.append(from_component)
    assm_to_filter.connections = new_connections

    # Cleanup 2: Remove components for which there is no connectors:
    component_names = list(
        set(component_names).intersection(set(components_with_connections))
    )
    filtered_components = OrderedDict()
    for name, c in assm_to_filter.components.items():
        if name in component_names:
            filtered_components[name] = c
    assm_to_filter.components = filtered_components
    return assm_to_filter


def assembly_not(filter1):
    def f(assm_to_filter):
        # Run the assembly through both filters to obtain new modified assemblies:
        assm_to_filter1 = prune(filter1.apply(copy.deepcopy(assm_to_filter)))

        # Union all component and connector names between two assemblies:
        component_names1 = {
            x.instance_name for x in assm_to_filter1.components.values()
        }
        connection_names1 = {x.name for x in assm_to_filter1.connections}

        # Anything in the original assembly that is not in the filtered assembly
        # we should keep:
        components = OrderedDict()
        for component in assm_to_filter.components.values():
            if component.instance_name not in component_names1:
                components[component.instance_name] = component
        connections = []
        for connection in assm_to_filter.connections:
            if connection.name not in connection_names1:
                connections.append(connection)

        # Create new assembly and return it:
        assm_to_filter.components = components
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def assembly_group(filter1, self):
    def f(assm_to_filter):
        # Run the assembly through both filters to obtain new modified assemblies:
        assm_to_filter1 = prune(filter1.apply(copy.deepcopy(assm_to_filter)))

        # Save off the connections into a group:
        self.groups.append(
            {c.instance_name: c for c in assm_to_filter1.components.values()}
        )
        return assm_to_filter1

    return f


def assembly_intersection(filter1, filter2):
    def f(assm_to_filter):
        # Run the assembly through both filters to obtain new modified assemblies:
        assm_to_filter1 = prune(filter1.apply(copy.deepcopy(assm_to_filter)))
        assm_to_filter2 = prune(filter2.apply(copy.deepcopy(assm_to_filter)))

        # Union all component and connector names between two assemblies:
        component_names1 = {
            x.instance_name for x in assm_to_filter1.components.values()
        }
        component_names2 = {
            x.instance_name for x in assm_to_filter2.components.values()
        }
        component_names_union = component_names1.intersection(component_names2)
        connection_names1 = {x.name for x in assm_to_filter1.connections}
        connection_names2 = {x.name for x in assm_to_filter2.connections}
        connection_names_union = connection_names1.intersection(connection_names2)

        # Gather all intersected component and connectors and create new assembly:
        components = OrderedDict()
        for component in assm_to_filter1.components.values():
            if component.instance_name in component_names_union:
                components[component.instance_name] = component
        connections = []
        for connection in assm_to_filter1.connections:
            if connection.name in connection_names_union:
                connections.append(connection)

        # Create new assembly and return it:
        assm_to_filter.components = components
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


def assembly_union(filter1, filter2):
    def f(assm_to_filter):
        # Run the assembly through both filters to obtain new modified assemblies:
        assm_to_filter1 = prune(filter1.apply(copy.deepcopy(assm_to_filter)))
        assm_to_filter2 = prune(filter2.apply(copy.deepcopy(assm_to_filter)))

        # Union all component and connector names between two assemblies:
        component_names1 = {
            x.instance_name for x in assm_to_filter1.components.values()
        }
        component_names2 = {
            x.instance_name for x in assm_to_filter2.components.values()
        }
        component_names_union = component_names1.union(component_names2)
        connection_names1 = {x.name for x in assm_to_filter1.connections}
        connection_names2 = {x.name for x in assm_to_filter2.connections}
        connection_names_union = connection_names1.union(connection_names2)

        # Gather all unioned component and connectors and create new assembly:
        components = OrderedDict()
        for component in list(assm_to_filter1.components.values()) + list(
            assm_to_filter2.components.values()
        ):
            if component.instance_name in component_names_union:
                components[component.instance_name] = component
                component_names_union.remove(component.instance_name)
        connections = []
        for connection in assm_to_filter1.connections + assm_to_filter2.connections:
            if connection.name in connection_names_union:
                connections.append(connection)
                connection_names_union.remove(connection.name)

        # Create new assembly and return it:
        assm_to_filter.components = components
        assm_to_filter.connections = connections
        return assm_to_filter

    return f


class Filter(object):
    def __init__(self, name, type, item_list=[], list_type="exclude"):
        self.name = name.lower()
        self.type = type.lower()

        if not item_list or not list_type:
            raise ModelException(
                'filter "'
                + self.name
                + '" must specify either an "include" and "exclude" list.'
            )

        self.list_type = list_type.lower()
        assert self.list_type in ["exclude", "include"]

        # Format lists correctly:
        if self.type in ["component_type", "connector_type", "component_type_context"]:
            self.item_list = list(set(map(lambda x: ada.formatType(x), item_list)))
        elif self.type in [
            "component_name",
            "connector_name",
            "component_name_context",
        ]:
            self.item_list = list(set(map(lambda x: ada.formatVariable(x), item_list)))
        elif self.type in ["component_execution", "component_kind", "connector_kind"]:
            self.item_list = list(set(map(lambda x: x.lower().strip(), item_list)))
        else:
            raise ModelException(
                'cannot process filter "'
                + self.name
                + '" of unrecognized type "'
                + self.type
                + '".'
            )

        # Additional checks for specific filter types:
        def check_enum(items, enum_list):
            for item in items:
                if item not in enum_list:
                    raise ModelException(
                        'filter "'
                        + self.name
                        + '" contains item "'
                        + item
                        + '" but can only contain: '
                        + str(enum_list)
                        + "."
                    )

        if self.type == "component_execution":
            check_enum(self.item_list, ["active", "passive"])
        if self.type == "component_kind":
            check_enum(self.item_list, ["queued", "simple"])
        if self.type == "connector_kind":
            check_enum(
                self.item_list,
                [
                    "send",
                    "recv_async",
                    "recv_sync",
                    "request",
                    "service",
                    "get",
                    "return",
                ],
            )

    def create_apply_function(self, assm):
        f = filter_type_dict[(self.type, bool(self.list_type == "include"))]
        return f(self, assm)

    @classmethod
    def from_filter_data(cls, filter_data):
        name = filter_data["name"]
        type = filter_data["type"]

        if "include" in filter_data and "exclude" in filter_data:
            raise ModelException(
                'filter "'
                + name
                + '" cannot specify both an "include" and "exclude" list. Please choose one.'
            )

        item_list = None
        list_type = None
        if "include" in filter_data:
            item_list = filter_data["include"]
            list_type = "include"
        elif "exclude" in filter_data:
            item_list = filter_data["exclude"]
            list_type = "exclude"

        return cls(name=name, type=type, item_list=item_list, list_type=list_type)


class view(models.base.base):
    """
    This is the object model for a view. It extracts data from a
    input file and stores the data as object member variables.
    """
    #################################################
    # View model:
    ##################################################

    def __init__(self, filename):
        """
        Initialize the view object, ingest data, and check it by
        calling the base class init function.
        """
        # This is sort of a hack, but allows us to not have to pass this class
        # everywhere just to print proper warnings. I would say that the use of
        # a global here is simpler than the visual noise that the alternative
        # would cause. Safety is not a concern, since this variable is only used
        # to print warnings.
        global MODEL_FILE
        MODEL_FILE = filename

        # Load the object from the file:
        super(view, self).__init__(filename, os.environ["SCHEMAPATH"] + "/view.yaml")

    def load(self):
        """Load view specific data structures with information from YAML file."""
        # Initialize object members:
        self.name = None
        self.prettyname = None
        self.description = None
        self.filters = OrderedDict()
        self.layout = "LR"

        self.show_switches = {}
        self.preamble = None
        self.postamble = None
        self.groups = []

        layout_dict = {
            "left-to-right": "LR",
            "top-to-bottom": "TB",
            "right-to-left": "RL",
            "bottom-to-top": "BT",
        }

        # Populate the object with the contents of the
        # file:
        if not self.specific_name:
            raise ModelException(
                "View filenames must be of the form 'specific_name.assembly_name.view.yaml'"
            )
        self.name = ada.adaCamelCase(self.specific_name)
        self.prettyname = self.name.replace("_", " ")
        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        if "postamble" in self.data:
            self.postamble = self.data["postamble"]
        if "layout" in self.data:
            self.layout = layout_dict[self.data["layout"]]

        # Show switches:
        def default_show_switches(keys):
            for k in keys:
                if k not in self.data:
                    self.data[k] = True
                self.show_switches[k] = self.data[k]

        default_show_switches(
            [
                "show_component_type",
                "show_component_kind",
                "show_component_execution",
                "show_component_priority",
                "show_component_name",
                "show_connector_type",
                "hide_group_outline",
            ]
        )

        if "filters" in self.data:
            for filter_data in self.data["filters"]:
                try:
                    filt = Filter.from_filter_data(filter_data)
                except ModelException as e:
                    raise ModelException(
                        'Error encountered while loading filter "'
                        + filter_data["name"]
                        + "': "
                        + str(e)
                    )

                if filt.name in self.filters:
                    raise ModelException(
                        'filter "'
                        + filt.name
                        + '" is a duplicate. Filter names must be unique.'
                    )
                else:
                    self.filters[filt.name] = filt

        # See if a rule is provided, if not create one:
        if "rule" in self.data:
            self.rule = self.data["rule"]
        else:
            self.rule = " & ".join([x.name for x in self.filters.values()])

    #################################################
    # Parsing stuff for view rules:
    ##################################################

    def _parseRule(self, assm):
        from pyparsing import (
            infixNotation,
            opAssoc,
            Word,
            alphas,
            alphanums,
            ParseException,
            Literal,
        )

        class FilterOp(object):
            def __init__(op_self, tokens):
                assert isinstance(tokens[0], str)
                op_self.filterName = tokens[0]
                if op_self.filterName not in self.filters:
                    raise ModelException(
                        'rule operates on filter "'
                        + op_self.filterName
                        + '" which is undefined.'
                    )
                op_self.apply = self.filters[op_self.filterName].create_apply_function(
                    assm
                )

            def __str__(op_self):
                return op_self.filterName

            __repr__ = __str__

        class NotOp(object):
            def __init__(op_self, tokens):
                arg = tokens[0]
                assert len(arg) == 2
                op_self.filter = arg[1]
                op_self.apply = assembly_not(op_self.filter)

            def __str__(op_self):
                return "not(" + str(op_self.filter) + ")"

            __repr__ = __str__

        class GroupOp(object):
            def __init__(op_self, tokens):
                arg = tokens[0]
                assert len(arg) == 2
                op_self.filter = arg[1]
                op_self.apply = assembly_group(op_self.filter, self)

            def __str__(op_self):
                return "group(" + str(op_self.filter) + ")"

            __repr__ = __str__

        class AndOp(object):
            def __init__(op_self, filterA, filterB):
                op_self.filterA = filterA
                op_self.filterB = filterB
                op_self.apply = assembly_intersection(op_self.filterA, op_self.filterB)

            def __str__(self):
                return "and(" + str(self.filterA) + ", " + str(self.filterB) + ")"

            __repr__ = __str__

        class AndOpChained(object):
            def __init__(op_self, tokens):
                args = tokens[0][0::2]
                op = args[0]
                for arg in args[1:]:
                    op = AndOp(op, arg)
                op_self.first_op = op
                op_self.apply = op_self.first_op.apply

            def __str__(op_self):
                return str(op_self.first_op)

            __repr__ = __str__

        class OrOp(object):
            def __init__(op_self, filterA, filterB):
                op_self.filterA = filterA
                op_self.filterB = filterB
                op_self.apply = assembly_union(op_self.filterA, op_self.filterB)

            def __str__(self):
                return "or(" + str(self.filterA) + ", " + str(self.filterB) + ")"

            __repr__ = __str__

        class OrOpChained(object):
            def __init__(op_self, tokens):
                args = tokens[0][0::2]
                op = args[0]
                for arg in args[1:]:
                    op = OrOp(op, arg)
                op_self.first_op = op
                op_self.apply = op_self.first_op.apply

            def __str__(op_self):
                return str(op_self.first_op)

            __repr__ = __str__

        # Define a graph name:
        filterOperand = Word(alphas + "_", alphanums + "_")
        filterOperand.setParseAction(FilterOp)

        # Define an expression as two operands
        # 'and'ed or 'or'ed together:
        NOT = Literal("~")
        GROUP = Literal("@")
        AND = Literal("&")
        OR = Literal("|")
        filterExpr = infixNotation(
            filterOperand,
            [
                (NOT, 1, opAssoc.RIGHT, NotOp),
                (GROUP, 1, opAssoc.RIGHT, GroupOp),
                (AND, 2, opAssoc.LEFT, AndOpChained),
                (OR, 2, opAssoc.LEFT, OrOpChained),
            ],
        )

        try:
            if self.rule:
                return filterExpr.parseString(self.rule)[0]
            else:
                return None
        except ParseException as err:
            raise ModelException(
                'Error parsing rule: "' + str(self.rule) + '".\n' + str(err)
            )

    def apply(self, assm):
        def new_assembly(data):
            """Create new assembly object from a YAML data object."""
            # Create new assembly
            a = models.assembly.assembly(filename=None, is_subassembly=True)
            # Add things to assembly from view to help out templates:
            a.is_view = True
            a.model_name = self.name
            a.layout = self.layout
            a.full_filename = "view model '" + self.name + "'"
            a.printMultiLine = models.base.printMultiLine
            a.data = data
            # Add all view "show" switches to assembly:
            a.show_switches = self.show_switches
            a.groups = self.groups
            # Load and check the new assembly:
            a.load()
            a.preamble = self.preamble
            a.postamble = self.postamble
            a.description = self.description
            # Set remaining components to aid in "group" graphing.
            component_names = set(c for comps in self.groups for c in comps.keys())
            a.remaining_components = [
                c
                for c in a.components.values()
                if c.instance_name not in component_names
            ]
            return a

        # We will apply changes to only the assm.data object and then we will
        # refresh the assembly object with this new data later.
        assm_copy = copy.deepcopy(assm)

        # Parse the rule to obtain a filter function:
        filt = self._parseRule(assm)
        # Apply the filter to the data:
        # import sys
        # sys.stderr.write("FILTER: " + str(filt) + "\n")
        if filt:
            new_assm = prune(filt.apply(assm_copy))
        else:
            new_assm = assm_copy

        # Create a new "data" dictionary from the new assembly
        # object. We do this in order to create a truly "fresh"
        # assembly object which includes only the filtered items.
        # Creating a "data" dictionary allows us to simulate the
        # loading of a YAML file from scratch with only the filtered
        # data in it.
        new_data = new_assm.data

        # Stuff filtered component data:
        new_component_data = []
        for component in new_assm.components.values():
            new_component_data.append(component.instance_data)
        new_data["components"] = new_component_data

        # Stuff filtered connection data:
        new_connection_data = []
        for connection in new_assm.connections:
            new_connection_data.append(connection.data)
        new_data["connections"] = new_connection_data

        # Remove any subassemblies, since our new data will already
        # contain subassembly components and connections, we don't want
        # to reload them:
        new_data["subassemblies"] = []

        # Create an assembly out of the new data and return it:
        return new_assembly(new_data)
