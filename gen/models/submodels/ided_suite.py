from util import ada
from models.submodels.variable import datatype
from collections import OrderedDict
from models.exceptions import ModelException, throw_exception_with_lineno
from models.base import renderable_object


class ided_entity(renderable_object):
    """
    Much of the functionality of Adamant is based off of
    entities that include and ID and a type, ie. commands,
    events, data products, etc. This base class holds the common
    functionality between those type of objects.
    """
    def __init__(
        self,
        name,
        type=None,
        description=None,
        id=None,
        default_value=None,
        variable_types_allowed=False,
        packed_types_required=False,
        suite=None,
    ):
        self.name = ada.formatType(name)
        self.id = id
        self.component = None
        # Make sure the ID won't exceed the Ada type used for IDs.
        if self.id is not None and self.id > 65535:
            raise ModelException(
                "Entity: '"
                + str(self.name)
                + " cannot be assigned ID "
                + str(id)
                + " because it is larger than 2**16-1."
            )
        self.description = description
        self.datatype = None
        self.default_value = default_value
        if type:
            self.datatype = datatype(type)

        # Some ided entities must have a packed type associated with them. If not they are invalid.
        if type and packed_types_required:
            if not self.has_packed_type:
                raise ModelException(
                    "Entity: '"
                    + str(self.name)
                    + "' uses the ordinary type '"
                    + str(self.type)
                    + "'. This entity type is required to use an Adamant packed type instead."
                )

        # We don't support variable length datatypes in ided entities
        # yet. So warn the user not to use that feature until it has
        # been implemented.
        if (
            self.type_model
            and self.type_model.variable_length
            and not variable_types_allowed
        ):
            raise ModelException(
                "Entity: '"
                + str(self.name)
                + "' uses variable length type '"
                + str(self.type)
                + "'. Variable length types are not (yet) supported. Consider using a static sized packed type instead."
            )

        # We don't allow volatile datatypes in ided entities.
        if type and self.datatype.is_volatile_type:
            raise ModelException(
                "Entity: '"
                + str(self.name)
                + "' specified Volatile, Atomic or Register type '"
                + str(self.type)
                + "' which is not allowed. Use a non-volatile type instead."
            )

        # Reference to suite that this entity if part of. This can be filled in
        # by the entity suite object itself.
        self.suite = suite
        if self.suite and hasattr(self.suite, "component") and self.suite.component:
            self.component = self.suite.component

        # Set size of ided entity using type model if it's available
        self.size = None
        if self.type_model:
            self.size = self.type_model.size

    @property
    def type(self):
        if self.datatype:
            return self.datatype.name
        return None

    @property
    def type_package(self):
        if self.datatype:
            return self.datatype.package
        return None

    @property
    def type_model(self):
        if self.datatype:
            return self.datatype.model
        return None

    @property
    def has_packed_type(self):
        if self.datatype:
            return self.datatype.is_packed_type
        return False

    @property
    def full_name(self, separator="."):
        component = None
        if self.component:
            component = self.component
        elif self.suite and hasattr(self.suite, "component") and self.suite.component:
            component = self.suite.component

        if component:
            if component.instance_name is not None:
                return component.instance_name + separator + self.name
            else:
                return component.name + separator + self.name
        else:
            return self.name

    def load_type_ranges(self):
        """
        Load the ranges for the type of this entity. This is not done by default because
        it can add a lot of time to the autocoding process. Only a few generators need
        the type ranges, so this is broken out into a function that can be used by those
        few generators.
        """
        if self.type_model:
            self.type_model.load_type_ranges()

    @classmethod
    @throw_exception_with_lineno
    def from_entity_data(
        cls,
        entity_data,
        type_name="type",
        variable_types_allowed=False,
        packed_types_required=False,
    ):
        name = entity_data["name"]
        type = None
        if type_name in entity_data:
            type = entity_data[type_name]
        description = None
        if "description" in entity_data:
            description = entity_data["description"]
        id = None
        if "id" in entity_data:
            id = entity_data["id"]
        default = None
        if "default" in entity_data:
            default = entity_data["default"]
        return cls(
            name=name,
            type=type,
            description=description,
            id=id,
            default_value=default,
            variable_types_allowed=variable_types_allowed,
            packed_types_required=packed_types_required,
        )


def _extract_from_suite_data(
    suite_data,
    entities_name,
    type_name,
    variable_types_allowed=False,
    packed_types_required=False,
):
    description = None
    if "description" in suite_data:
        description = suite_data["description"]
    # Grab all the entities from the suite data:
    entities = []
    for entity_data in suite_data[entities_name]:
        entities.append(
            ided_entity.from_entity_data(
                entity_data, type_name, variable_types_allowed, packed_types_required
            )
        )
    return entities, description


class ided_suite(renderable_object):
    """A class which holdes a suite (collection) of ided entities."""
    def __init__(
        self, name=None, entity_name=None, entities=[], description=None, component=None
    ):
        if name and entity_name and entities:
            self._initialize(name, entity_name, entities, description, component)

    def _initialize(
        self, name, entity_name, entities, description=None, component=None
    ):
        self.id_base = None
        self.name = name
        self.entity_name = entity_name
        self.description = description
        self.entities = OrderedDict()
        self.component = component
        for entity in entities:
            if entity.name in self.entities:
                raise ModelException(
                    "Found entities with duplicate name: '"
                    + str(entity.name)
                    + "'. All entity names must be unique."
                )
            entity.suite = self
            entity.component = self.component
            self.entities[entity.name] = entity

        # Get a list of all the ids and make sure there are no duplicates:
        self.ids = [e.id for e in self.entities.values() if e.id]
        for id in self.ids:
            if self.ids.count(id) > 1:
                raise ModelException(
                    "Found entities with duplicate id: '"
                    + str(id)
                    + "'. All identifiers must be unique."
                )

        # Make sure that all entities either have an id, or do not have an id. A suite must
        # be contain either all ided entities, or none.
        if len(self.ids) > 0 and len(self.ids) != len(self.entities):
            raise ModelException(
                "The suite must contain ids for ALL entities or NO entities. Ids have been "
                + "provided for some entities but not others, which is forbidden."
            )

        # Store the includes necessary to include the entity types:
        self.includes = list(
            OrderedDict.fromkeys(
                [e.type_package for e in self.entities.values() if e.type_package]
            )
        )
        self.representation_includes = list(
            OrderedDict.fromkeys(
                [
                    (e.type_package + ".Representation")
                    if e.type_model
                    else e.type_package
                    for e in self.entities.values()
                    if e.type_package
                ]
            )
        )

        # Store the includes for any complex types (those that have models).
        self.complex_type_includes = list(
            OrderedDict.fromkeys(
                [e.type_package for e in self.entities.values() if e.type_model]
            )
        )

        # Store the types in a list:
        self.types = list(
            OrderedDict.fromkeys([e.type for e in self.entities.values() if e.type])
        )

        # Store the variable length types in a list:
        self.variable_length_types = list(
            OrderedDict.fromkeys(
                [
                    e.type
                    for e in self.entities.values()
                    if e.type_model and e.type_model.variable_length
                ]
            )
        )

        # Store the basic types in a list:
        self.basic_types = list(
            OrderedDict.fromkeys(
                [e.type for e in self.entities.values() if e.type and not e.type_model]
            )
        )

        # Store the type models in a list:
        self.type_models = list(
            OrderedDict.fromkeys(
                [e.type_model for e in self.entities.values() if e.type_model]
            )
        )

        # Store the type model files in a list:
        self.deps_list = [e.full_filename for e in self.type_models]
        for m in self.type_models:
            self.deps_list += [m.full_filename] + m.get_dependencies()
        self.deps_list = list(set(self.deps_list))

    def __nonzero__(self):
        return bool(self.entities)

    def __iter__(self):
        return iter(self.entities.values())

    def __len__(self):
        return len(self.entities)

    def get_dependencies(self):
        return self.deps_list

    def names(self):
        return list(self.entities.keys())

    def id_base_parameter_name(self):
        if self.entity_name.endswith("s"):
            name = self.entity_name[:-1]
        else:
            name = self.entity_name
        return ada.formatVariable(name) + "_Id_Base"

    def _set_ids(self, start_id):
        for entity in self.entities.values():
            if entity.id is None:
                entity.id = start_id
                start_id += 1

                # Make sure the ID won't exceed the Ada type used for IDs.
                if entity.id > 65535:
                    raise ModelException(
                        "Entity: '"
                        + str(entity.name)
                        + " cannot be assigned ID "
                        + str(start_id)
                        + " because it is larger than 2**16-1."
                    )
            else:
                raise ModelException(
                    "Cannot set id of entity: '"
                    + str(entity.name)
                    + "' of '"
                    + str(self.name)
                    + "' to "
                    + str(start_id)
                    + " because it is already set to "
                    + str(entity.id)
                    + "."
                )

    def set_id_base(self, start_id):
        self.id_base = start_id
        self._set_ids(self.id_base)

        # Set the parameter value in the component subprogram:
        if self.component:
            # Make sure, if an id is set, that it is the same id we are getting ready to set, otherwise
            # this is a bug. If it is not set yet, then set it.
            parameter_name = self.id_base_parameter_name()
            value = self.component.set_id_bases.get_parameter_value(parameter_name)
            if value:
                assert value == str(self.id_base), "This should never happen."
            else:
                self.component.set_id_bases.set_parameter_value(
                    self.id_base_parameter_name(), str(self.id_base)
                )

    def get_with_name(self, entity_name):
        try:
            return self.entities[ada.formatType(entity_name)]
        except KeyError:
            return None

    @classmethod
    @throw_exception_with_lineno
    def from_suite_data(
        cls,
        suite_name,
        suite_data,
        entities_name,
        type_name,
        variable_types_allowed=False,
        packed_types_required=False,
    ):
        entities, description = _extract_from_suite_data(
            suite_data,
            entities_name,
            type_name,
            variable_types_allowed,
            packed_types_required,
        )
        return cls(
            name=suite_name,
            entity_name=entities_name,
            entities=entities,
            description=description,
        )

    @throw_exception_with_lineno
    def load_suite_data(
        self,
        suite_name,
        suite_data,
        entities_name,
        type_name,
        variable_types_allowed=False,
        packed_types_required=False,
    ):
        entities, description = _extract_from_suite_data(
            suite_data,
            entities_name,
            type_name,
            variable_types_allowed,
            packed_types_required,
        )
        self._initialize(
            name=suite_name,
            entity_name=entities_name,
            entities=entities,
            description=description,
        )
