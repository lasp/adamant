from models.exceptions import ModelException
from models.submodels.variable import variable
from models.submodels.parameter import parameter
from models.submodels.subprogram import subprogram
from models.exceptions import throw_exception_with_lineno
from util import ada
from collections import OrderedDict
import copy

# Private functions and definitions useful for
# component loading.
_connector_kind_map = {
    "send": ["recv_sync", "recv_async"],
    "recv_sync": ["send"],
    "recv_async": ["send"],
    "request": ["service"],
    "service": ["request"],
    "return": ["get"],
    "get": ["return"],
    "provide": ["modify"],
    "modify": ["provide"],
}
_connector_direction_map = {
    "invoker": ["send", "request", "get", "provide"],
    "invokee": ["recv_async", "recv_sync", "service", "return", "modify"],
}
_connector_mode_map = {
    "request": "in",
    "service": "in",
    "get": "out",
    "return": "out",
    "send": "in",
    "recv_async": "in",
    "recv_sync": "in",
    "provide": "in out",
    "modify": "in out",
}
_connector_package_map = {
    "request": "In_Return_Connector",
    "service": "In_Return_Connector",
    "get": "Return_Connector",
    "return": "Return_Connector",
    "send": "In_Connector",
    "recv_async": "In_Connector",
    "recv_sync": "In_Connector",
    "provide": "In_Out_Connector",
    "modify": "In_Out_Connector",
}
_kind_direction_map = {
    "request": "invoker",
    "service": "invokee",
    "get": "invoker",
    "return": "invokee",
    "send": "invoker",
    "recv_async": "invokee",
    "recv_sync": "invokee",
    "provide": "invoker",
    "modify": "invokee",
}

# Common connector list - The following is a list of common connectors found in Adamant. The list
# here should exactly match the connectors listed in src/core/connector/common_connectors.ads. This
# mapping needs to maintained manually (with the current implementation).
#
# The purpose of this list is to consolidate instantiation
# of these generic packages into a single place that can be used by multiple
# component. Doing this provides a huge savings on the resulting code size
# of the compiled binary, by not duplicating generic connector instantiations.
#
# Connectors will be marked on ingest if they belong to this common connector list, and thus
# indicating that they can use the common generic instantiation in the autocode instead of
# declaring their own.
_common_connector_list = [
    "Sys_Time_T_Return_Connector",
    "Data_Product_T_In_Connector",
    "Command_T_In_Connector",
    "Command_Response_T_In_Connector",
    "Event_T_In_Connector",
    "Tick_T_In_Connector",
    "Packet_T_In_Connector",
    "Ccsds_Space_Packet_T_In_Connector",
    "Parameter_Update_T_In_Out_Connector",
    "Control_Input_U_In_Connector",
    "Control_Output_U_In_Connector",
    "Pet_T_In_Connector",
    "Fault_T_In_Connector",
    "Data_Product_Fetch_T_In_Return_Connector",
]


def _get_connector_tester_name(connector_name, kind):
    normal_suffix = "_" + ada.adaCamelCase(kind)
    if connector_name.endswith(normal_suffix):
        return (
            connector_name[0:len(connector_name) - len(normal_suffix)]
            + "_"
            + ada.adaCamelCase(_connector_kind_map[kind][0])
        )
    return connector_name + "_Reciprocal"


def _check_kind(name, kind, type, return_type):
    def check_not_none(var, field_name):
        if not var:
            raise ModelException(
                "Connector '"
                + name
                + "' is of kind '"
                + kind
                + "' which requires the field: '"
                + field_name
                + "'"
            )

    def check_none(var, field_name):
        if var:
            raise ModelException(
                "Connector '"
                + name
                + "' is of kind '"
                + kind
                + "' which forbids the field: '"
                + field_name
                + "'"
            )

    if kind in ["send", "recv_sync", "recv_async", "provide", "modify"]:
        check_not_none(type, "type")
        check_none(return_type, "return_type")
    elif kind in ["request", "service"]:
        check_not_none(type, "type")
        check_not_none(return_type, "return_type")
    elif kind in ["get", "return"]:
        check_none(type, "type")
        check_not_none(return_type, "return_type")
    else:
        raise ModelException(
            "Connector '" + name + "' includes an unrecognized kind '" + kind + "'."
        )


class connector(subprogram):
    """
    This model holds a connector and allows
    easy access to its members:
    """
    # To initialize the component object, connector data must be
    # passed in.
    def __init__(
        self,
        kind,
        type=None,
        return_type=None,
        name=None,
        count=1,
        priority=None,
        description=None,
    ):
        if not type and not return_type:
            raise ModelException(
                "Connectors must specify either a 'type' or return_type'."
            )

        self._kind = kind.lower()
        self._direction = _kind_direction_map[self._kind]
        self._tester_kind = _connector_kind_map[self._kind][0]
        self._count = count
        self._priority = priority
        self._generic = False
        self._connector_package = _connector_package_map[self._kind]
        self._connected_to = [None] * count  # List of connected connector objects
        self._connections = [None] * count  # List of connected connection objects
        self._unconstrained = False
        if self._count == 0:
            self._unconstrained = True

        # Compute the type string base on the connector's type(s):
        type_string = None
        if type:
            type_string = type.replace(".", "_")
        elif return_type:
            type_string = return_type.replace(".", "_")
        else:
            assert False, "should never happen"

        # Compute the connector name:
        if name:
            connector_name = ada.formatType(name)
        else:
            connector_name = ada.formatType(type_string + "_" + self._kind)
        self._tester_name = _get_connector_tester_name(connector_name, self._kind)

        # Make sure that the required values for kind are provided:
        _check_kind(connector_name, self._kind, type, return_type)

        # Arrayed connectors are only supported for invoker connectors:
        # if kind in ["recv_sync", "recv_async", "modify", "service"] and self._count != 1:
        #   raise ModelException("Arrayed invokee connectors not supported.")
        # ^ This feature is now supported as of 8/20/2021

        # Priorities are only supported for async_recv connectors. Make sure the user didn't
        # assign a priority to a non async_recv connector.
        if kind == "recv_async":
            # Zero is default priority if none is specified. Make it so.
            if self._priority is None:
                self._priority = 0
        else:
            if self._priority is not None:
                raise ModelException(
                    "A priority can only be specified for connectors of kind 'recv_async'."
                )

        # Add subprogram parameters for connectors:
        parameters = []
        if type:
            mode = _connector_mode_map[self._kind]
            parameters.append(
                parameter(
                    name="arg",
                    type=type,
                    mode=mode,
                    description="Argument for connector " + connector_name + ".",
                )
            )
            # Make sure the type used for the connector is not a volatile type. This is not allowed.
            if parameters[0].is_volatile_type:
                raise ModelException(
                    "Connector: '"
                    + connector_name
                    + "' cannot contain the Volatile, Atomic, or Register type '"
                    + str(type)
                    + "'."
                )
        return_value = None
        if return_type:
            return_value = variable(
                "to_Return",
                type=return_type,
                description="Return value of connector " + connector_name + ".",
            )
            if return_value.is_volatile_type:
                raise ModelException(
                    "Connector: '"
                    + connector_name
                    + "' cannot contain the Volatile, Atomic, or Register return type '"
                    + str(return_type)
                    + "'."
                )

        # Instantiate subprogram base class:
        assert (
            len(parameters) >= 0 and len(parameters) <= 1
        ), "Connectors should always have only 0 or 1 parameters."
        super(connector, self).__init__(
            name=connector_name,
            description=description,
            parameters=parameters,
            return_value=return_value,
        )

        # See if connector is of a common kind and type:
        self._common = False
        self._common_connector_package = ada.formatType(
            type_string + "_" + self._connector_package
        )
        if self._common_connector_package in _common_connector_list:
            self._common = True

        # Add to includes:
        self._type_includes = copy.deepcopy(self._includes)
        self._type_representation_includes = copy.deepcopy(
            self._representation_includes
        )
        if self._common:
            self._includes.append("Common_Connectors")
        else:
            self._includes.append(self._connector_package)

    @classmethod
    @throw_exception_with_lineno
    def from_connector_data(cls, connector_data):
        kind = connector_data["kind"]
        type = None
        return_type = None
        count = 1
        priority = None
        description = None
        name = None

        if "type" in connector_data:
            type = connector_data["type"]
        if "return_type" in connector_data:
            return_type = connector_data["return_type"]
        if "count" in connector_data:
            count = int(connector_data["count"])
        if "priority" in connector_data:
            priority = int(connector_data["priority"])
        if "description" in connector_data:
            description = connector_data["description"]
        if "name" in connector_data:
            name = connector_data["name"]

        return cls(
            kind=kind,
            type=type,
            return_type=return_type,
            count=count,
            priority=priority,
            description=description,
            name=name,
        )

    ################################
    # Properties:
    ################################
    @property
    def kind(self):
        return self._kind

    @property
    def direction(self):
        return self._direction

    @property
    def tester_kind(self):
        return self._tester_kind

    @property
    def tester_name(self):
        return self._tester_name

    @tester_name.setter
    def tester_name(self, name):
        self._tester_name = name

    @property
    def count(self):
        return self._count

    @property
    def priority(self):
        return self._priority

    @property
    def unconstrained(self):
        return self._unconstrained

    @property
    def generic(self):
        return self._generic

    @property
    def generic_serialized_length_func(self):
        return self._generic_serialized_length_func

    @property
    def generic_types(self):
        types = []
        if self._generic:
            if self.type:
                types.append(self.type)
            if self.return_type:
                types.append(self.return_type)
            return types
        return []

    @property
    def connector_package(self):
        return self._connector_package

    @property
    def parameter(self):
        try:
            return self.parameters[0]
        except IndexError:
            return None

    @property
    def parameter_name(self):
        try:
            return self.parameter_names[0]
        except IndexError:
            return None

    @property
    def type(self):
        if self.parameter:
            return self.parameter.type
        else:
            return None

    @property
    def type_package(self):
        if self.parameter:
            return self.parameter.type_package
        else:
            return None

    @property
    def type_model(self):
        if self.parameter:
            return self.parameter.type_model
        else:
            return None

    @property
    def mode(self):
        return self.parameter.mode

    @property
    def type_includes(self):
        """Return unique include list."""
        return self._type_includes

    @property
    def type_representation_includes(self):
        return self._type_representation_includes

    @property
    def common(self):
        return self._common

    @property
    def common_connector_package(self):
        return self._common_connector_package

    ################################
    # Methods:
    ################################
    def set_count(self, count):
        if self._unconstrained:
            if self._count == 0:
                if count > 0:
                    self._count = count
                else:
                    raise ModelException(
                        "Connector: '"
                        + self._name
                        + "' cannot be set to '"
                        + str(count)
                        + "'. It must be set to a value of at least 1."
                    )
            else:
                raise ModelException(
                    "Connector: '"
                    + self._name
                    + "' has already been set with a count of '"
                    + str(self._count)
                    + "'. Cannot set to '"
                    + str(count)
                    + "'."
                )
        else:
            raise ModelException(
                "Connector: '"
                + self._name
                + "' is constrained to a count of '"
                + str(self._count)
                + "'. Cannot change count to '"
                + str(count)
                + "'."
            )

        # Allocate proper size for internal connected_to variable:
        assert not self._connected_to, "This would be a bug."
        self._connected_to = [None] * self._count
        self._connections = [None] * self._count

    def set_type_generic(self, serialized_length_func=False):
        self.parameter.set_generic()
        self._generic = True
        self._generic_serialized_length_func = serialized_length_func

    def set_return_type_generic(self, serialized_length_func=False):
        self._return_value.set_generic()
        self._generic = True
        self._generic_serialized_length_func = serialized_length_func

    def is_type_generic(self):
        if self.parameter:
            return self.parameter.generic
        return False

    def has_generic_serialized_length_func(self):
        return bool(self._generic_serialized_length_func)

    def is_return_type_generic(self):
        if self._return_value:
            return self._return_value.generic
        return False

    def set_generic_type(self, type):
        if self._generic and self.parameter.generic:
            if self.parameter and ada.formatType(self.parameter.type):
                self.parameter.set_type(type)
        else:
            raise ModelException(
                "Connector: '"
                + self._name
                + "' type is not generic. Type cannot be set to '"
                + str(type)
                + "'."
            )

    def set_generic_return_type(self, type):
        if self._generic and self._return_value.generic:
            if self._return_value and self._return_value.type:
                self._return_value.set_type(type)
        else:
            raise ModelException(
                "Connector: '"
                + self._name
                + "' return type is not generic. Return type cannot be set to '"
                + str(type)
                + "'."
            )

    def connected(self, index=1):
        assert index > 0, "1-indexed"
        conn = self._connected_to[index - 1]
        return bool(conn is not None and conn != "ignore")

    def ignored(self, index=1):
        assert index > 0, "1-indexed"
        conn = self._connected_to[index - 1]
        return bool(conn == "ignore")

    def get_connected(self):
        return self._connector_to

    def get_connections(self):
        return self._connections

    def connect_to(self, connector, connection, from_index=1, to_index=1):
        def err(msg):
            raise ModelException(
                "Connector: '"
                + self._name
                + "' at index '"
                + str(from_index)
                + "' cannot be connected to '"
                + connector.name
                + "' at index '"
                + str(to_index)
                + "': "
                + msg
            )

        assert from_index > 0, "1-indexed"
        assert to_index > 0, "1-indexed"

        # Basic connection checks:
        if self._direction == "invokee":
            raise err("Connections must be made from invoker to invokee.")
        if not self._connected_to:
            raise err(
                "Connector '"
                + self.name
                + "' is an arrayed connector with unspecified 'count'."
            )
        if not connector._connected_to:
            raise err(
                "Connector '"
                + connector.name
                + "' is an arrayed connector with unspecified 'count'."
            )
        # Make sure the connectors are compatible:
        if self._direction == connector.direction:
            err(
                "Incompatible 'directions': "
                + str(self._direction)
                + " and "
                + str(connector.direction)
                + ". Connections must be made from invoker to invokee."
            )
        if self._kind not in _connector_kind_map[connector.kind]:
            err(
                "Incompatible 'kinds': "
                + str(self._kind)
                + " and "
                + str(connector.kind)
                + "."
            )
        if self.type != connector.type:
            err(
                "Incompatible 'types': "
                + str(self.type)
                + " and "
                + str(connector.type)
                + "."
            )
        if self.return_type != connector.return_type:
            err(
                "Incompatible 'return_types': "
                + str(self.return_type)
                + " and "
                + str(connector.return_type)
                + "."
            )

        # Connect invoker to invokee:
        try:
            if self._connected_to[from_index - 1]:
                try:
                    raise err(
                        "From connector is already connected to '"
                        + self._connected_to[from_index - 1].name
                        + "'."
                    )
                except AttributeError:
                    raise err(
                        "From connector is already connected to '"
                        + str(self._connected_to[from_index - 1])
                        + "'."
                    )
            self._connected_to[from_index - 1] = connector
            self._connections[from_index - 1] = connection
        except IndexError:
            err(
                "From index '"
                + str(from_index)
                + "' does not exist in connector. Connector array has length of '"
                + str(self._count)
                + "'."
            )

        # Connect invokee to invoker:
        try:
            if connector._connected_to[to_index - 1] == "ignore":
                raise err(
                    "Invokee connector is ignored. Remove the ignored connection to connect this connector."
                )
            # We can have more than one invoker connected to a single invoker, so we use an
            # array of arrays. The first index is the connector index, which holds a list
            # of the invokees attached.
            try:
                connector._connected_to[to_index - 1].append(self)
                connector._connections[to_index - 1].append(connection)
            except AttributeError:
                connector._connected_to[to_index - 1] = [self]
                connector._connections[to_index - 1] = [connection]
        except IndexError:
            err(
                "To index '"
                + str(to_index)
                + "' does not exist in connector. Connector array has length of '"
                + str(connector._count)
                + "'."
            )

    def ignore(self, index=1):
        def err(msg):
            raise ModelException(
                "Connector: '"
                + self._name
                + "' cannot be ignored at index '"
                + str(index)
                + "': "
                + msg
            )

        assert index > 0, "1-indexed"

        # Basic connection checks:
        if not self._connected_to:
            raise err(
                "Connector '"
                + self.name
                + "' is an arrayed connector with unspecified 'count'."
            )

        # Connect invokee to invoker:
        if self._direction == "invoker":
            try:
                if self._connected_to[index - 1]:
                    if self._connected_to[index - 1] == "ignore":
                        err("Invoker connector already ignored.")
                    else:
                        err(
                            "It is already connected to '"
                            + self._connected_to[index - 1].name
                            + "'."
                        )
                self._connected_to[index - 1] = "ignore"
                self._connections[index - 1] = "ignore"
            except IndexError:
                err(
                    "Index '"
                    + str(index)
                    + "' does not exist in connector. Connector array has length of '"
                    + str(self._count)
                    + "'."
                )
        # Connect invokee to invoker:
        else:
            if self._connected_to[index - 1] is not None:
                if self._connected_to[index - 1] == "ignore":
                    err("Invokee connector already ignored.")
                else:
                    err(
                        "Invokee connector connected to "
                        + str([c.name for c in self._connected_to[index - 1]])
                    )
            else:
                self._connected_to[index - 1] = "ignore"
                self._connections[index - 1] = "ignore"


class connectors(object):
    """Component interface class, holds a set of connectors."""
    def __init__(self, connector_dict=OrderedDict(), create_cache=False):
        self._connectors = connector_dict
        self._cache = {}

        # Pre-compute useful dictionaries. This is a performance enhancement
        # that saves repeated computations when calling the methods below in
        # a template.
        if create_cache:
            for kind in _connector_kind_map.keys():
                self.of_kind(kind)
            self.invoker()
            self.invokee()
            self.n_arrayed()
            self.non_arrayed()
            self.arrayed()
            self.max()
            self.generic()
            self.generic_types()

    def __nonzero__(self):
        return bool(self._connectors)

    def __iter__(self):
        return iter(self._connectors.values())

    def __len__(self):
        return len(self._connectors)

    @classmethod
    def from_connector_list(cls, connectors, create_cache=False):
        # Create connectors dict from list:
        connector_dict = OrderedDict()
        for con in connectors:
            # Resolve any name conflicts:
            while con.name in connector_dict:
                last_letter = con.name[-1]
                second_to_last_letter = con.name[-2]
                if last_letter.isdigit() and second_to_last_letter == "_":
                    to_add = "_" + str(int(last_letter) + 1)
                    con.name = con.name[:-2] + to_add
                    con.tester_name = con.tester_name[:-2] + to_add
                else:
                    con.name = con.name + "_2"
                    con.tester_name = con.tester_name + "_2"

            # Store connector with unique name:
            connector_dict[con.name] = con

            # Sometimes connectors can still have duplicate names in
            # the tester. Let's resolve these too.
            tester_names = []
            for con in connector_dict.values():
                while con.tester_name in tester_names:
                    last_letter = con.tester_name[-1]
                    second_to_last_letter = con.tester_name[-2]
                    if last_letter.isdigit() and second_to_last_letter == "_":
                        to_add = "_" + str(int(last_letter) + 1)
                        con.tester_name = con.tester_name[:-2] + to_add
                    else:
                        con.tester_name = con.tester_name + "_2"
                tester_names.append(con.tester_name)

        return cls(connector_dict, create_cache=create_cache)

    @staticmethod
    def from_component_data(component_data, create_cache=True):
        if "connectors" in component_data:
            connector_list = []
            for connector_data in component_data["connectors"]:
                connector_list.append(connector.from_connector_data(connector_data))
            return connectors.from_connector_list(
                connector_list, create_cache=create_cache
            )
        else:
            return None

    ################################
    # Properties:
    ################################
    @property
    def list(self):
        return self._connectors.values()

    @property
    def names(self):
        return self._connectors.keys()

    @property
    def includes(self):
        """Return unique include list."""
        includes = []
        for con in self._connectors.values():
            includes.extend(con.includes)
        return list(OrderedDict.fromkeys(includes))

    @property
    def includes_for_generic(self):
        """
        Generic components cannot use the Common_Connectors package, and
        instead must use the connector specific connector package.
        """
        includes = []
        for con in self._connectors.values():
            includes.extend(con.includes + [con.connector_package])
        to_return = list(OrderedDict.fromkeys(includes))
        try:
            to_return.remove("Common_Connectors")
        except Exception:
            pass
        return to_return

    @property
    def type_includes(self):
        """Return unique include list."""
        includes = []
        for con in self._connectors.values():
            includes.extend(con.type_includes)
        return list(OrderedDict.fromkeys(includes))

    @property
    def type_representation_includes(self):
        includes = []
        for con in self._connectors.values():
            includes.extend(con.type_representation_includes)
        return list(OrderedDict.fromkeys(includes))

    @property
    def types(self):
        """Return unique include list."""
        types = []
        for con in self._connectors.values():
            if con.type:
                types.append(con.type)
            if con.return_type:
                types.append(con.return_type)
        return list(OrderedDict.fromkeys(types))

    @property
    def basic_types(self):
        """Return all connector types"""
        types = []
        for con in self._connectors.values():
            # Variable sized generics are not treated like basic types in the autocode.
            if not (con.generic and con.generic_serialized_length_func):
                types.extend(con.basic_types)
        return list(OrderedDict.fromkeys(types))

    @property
    def type_models(self):
        type_models = []
        for con in self._connectors.values():
            type_models.extend(con.type_models)
        return list(OrderedDict.fromkeys(type_models))

    @property
    def variable_length_types(self):
        """Return all variable length types"""
        types = []
        for type_model in self.type_models:
            if type_model.variable_length:
                types.append(type_model.name)
        return list(OrderedDict.fromkeys(types))

    ################################
    # Private methods:
    ################################
    def _of_direction(self, direction):
        if direction not in self._cache:
            # Sort connectors 'kind' based dictionaries:
            con_dict = OrderedDict()
            for con in self._connectors.values():
                if con.direction == direction:
                    con_dict[con.name] = con
            self._cache[direction] = connectors(con_dict)
        return self._cache[direction]

    def _of_count(self, count):
        con_dict = OrderedDict()
        for con in self._connectors.values():
            if con.count == count:
                con_dict[con.name] = con
        return connectors(con_dict)

    ################################
    # Useful filters:
    ################################
    # Each of these methods return a new connections object with
    # some of the connections removed. The connections kept/removed
    # is determined by the function called and the parameters passed.
    def of_kind(self, kind):
        if kind not in self._cache:
            # Sort connectors 'kind' based dictionaries:
            con_dict = OrderedDict()
            the_kind = kind.lower()
            assert the_kind in _connector_kind_map, "Kind must be one of: " + str(
                list(_connector_kind_map.keys())
            )
            for con in self._connectors.values():
                if con.kind == the_kind:
                    con_dict[con.name] = con
            self._cache[kind] = connectors(con_dict)
        return self._cache[kind]

    def invoker(self):
        return self._of_direction("invoker")

    def invokee(self):
        return self._of_direction("invokee")

    def n_arrayed(self):
        if "n_arrayed" not in self._cache:
            self._cache["n_arrayed"] = self._of_count(0)
        return self._cache["n_arrayed"]

    def non_arrayed(self):
        if "non_arrayed" not in self._cache:
            self._cache["non_arrayed"] = self._of_count(1)
        return self._cache["non_arrayed"]

    def arrayed(self):
        if "arrayed" not in self._cache:
            con_dict = OrderedDict()
            for con in self._connectors.values():
                if con.count != 0 and con.count != 1:
                    con_dict[con.name] = con
            self._cache["arrayed"] = connectors(con_dict)
        return self._cache["arrayed"]

    def arrayed_invokee(self):
        return self.invokee().arrayed()

    def requires_queue(self):
        """
        Returns True if the connectors specify that the component needs
        an internal queue, otherwise returns false:
        """
        return bool(self.of_kind("recv_async"))

    def requires_priority_queue(self):
        """
        Returns True if the connectors specify that the component needs
        an internal priority queue, otherwise returns false:
        """
        recv_async_connectors = self.of_kind("recv_async")
        if recv_async_connectors:
            priorities = [conn.priority for conn in recv_async_connectors]
            # If all the priorities are not the same then we need a priority queue
            return not bool(len(set(priorities)) == 1)
        return False

    def max(self):
        """
        Return either the set of invokee connectors,
        or the set of invoker connectors, whichever has
        more.
        """
        if "max" not in self._cache:
            invokee = self.invokee()
            invoker = self.invoker()
            if len(invokee) > len(invoker):
                self._cache["max"] = invokee
            else:
                self._cache["max"] = invoker
        return self._cache["max"]

    def generic(self):
        if "generic" not in self._cache:
            con_dict = OrderedDict()
            for con in self._connectors.values():
                if con.generic:
                    con_dict[con.name] = con
            self._cache["generic"] = connectors(con_dict)
        return self._cache["generic"]

    def generic_types(self):
        if "generic_types" not in self._cache:
            types = []
            for con in self._connectors.values():
                types.extend(con.generic_types)
            self._cache["generic_types"] = types
        return self._cache["generic_types"]

    def of_name(self, name):
        """Non cached filters."""
        name = ada.formatType(name)
        try:
            return self._connectors[name]
        except KeyError:
            raise ModelException("No connectors exist of name: '" + str(name) + "'.")

    def common(self):
        if "common" not in self._cache:
            con_dict = OrderedDict()
            for con in self._connectors.values():
                if con.common:
                    con_dict[con.name] = con
            self._cache["common"] = connectors(con_dict)
        return self._cache["common"]
