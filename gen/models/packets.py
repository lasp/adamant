from models.component import component_submodel
from util import ada
from collections import OrderedDict
from models.submodels.ided_suite import ided_suite, ided_entity
from models.submodels.parameter import parameter
from models.submodels.field import field
import os.path


# A flattened packet item is a "field" with a few extra capabilities:
class packet_item(field):
    def __init__(self, data, name, start_bit, entity=None, description=None):
        self.__dict__ = data
        self.packet_start_bit = start_bit
        self.packet_end_bit = start_bit + self.size - 1
        self.entity = entity
        self.flat_desc = description
        self.flat_name = name

    @classmethod
    def from_field(cls, from_field, name, start_bit, entity, description):
        import copy

        data = copy.copy(from_field.__dict__)
        return cls(data, name, start_bit, entity, description)

    @property
    def flattened_description(self):
        return (
            (
                self.entity.full_name + " - " + self.entity.description
                if self.entity and self.entity.description
                else ""
            )
            + "\n."
            + self.flat_desc
        )

    @property
    def full_name(self):
        return (self.entity.full_name + "." if self.entity else "") + self.flat_name

    # By loading the containing object's type ranges the packet item (which is a field of the
    # containing object's packed type) will get its type ranges filled in.
    def load_type_ranges(self):
        self.entity.load_type_ranges()


def _items_from_record(record, start_bit=0, container_obj=None):
    names = record.flattened_names()
    items = record.flatten()
    packet_items = []
    descriptions = record.flattened_descriptions(separator="\n.")
    bit = start_bit
    for item, name, desc in zip(items, names, descriptions):
        new_item = packet_item.from_field(
            from_field=item,
            name=name,
            start_bit=bit,
            entity=container_obj,
            description=desc,
        )
        packet_items.append(new_item)
        bit += new_item.size
    packet_item_names = [item.full_name for item in packet_items]
    return OrderedDict(zip(packet_item_names, packet_items)), bit


# Create a list of flattened packet items from an IDed Entity with a type.
def items_list_from_ided_entity(entity, start_bit=0):
    items = []
    names = []

    # Create items list:
    if entity.type:
        if entity.type_model:
            return _items_from_record(
                entity.type_model, start_bit=start_bit, container_obj=entity
            )
        else:
            items.append(entity.type)
            names.append(entity.full_name + "." + entity.name)

    return OrderedDict(zip(names, items)), start_bit


# A packet is an IDed entity that also includes an attribute called "items" which list
# the flattened packet type.
class packet(ided_entity):
    def __init__(self, name, type=None, description=None, id=None, suite=None):
        super(packet, self).__init__(
            name,
            type,
            description,
            id,
            default_value=None,
            variable_types_allowed=True,
            suite=suite,
        )
        self.items, ignore = items_list_from_ided_entity(self)
        self.type_ranges_loaded = False

    @classmethod
    def from_ided_entity(cls, entity):
        self = entity
        self.items, ignore = items_list_from_ided_entity(self)
        return self

    # Special load type ranges that also makes sure the type ranges are loaded for all
    # the packet items.
    def load_type_ranges(self):
        if not self.type_ranges_loaded:
            # Call the base class type ranges load:
            super(packet, self).load_type_ranges()
            self.type_ranges_loaded = True
            # Load the type ranges into each item
            for item in self.items.values():
                item.load_type_ranges()


# This is the object model for a data product suite. It extracts data from a
# input file and stores the data as object member variables.
class packets(component_submodel, ided_suite):
    # Initialize the data product object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        ided_suite.__init__(self)
        component_submodel.__init__(
            self, filename=filename, schema=os.environ["SCHEMAPATH"] + "/packets.yaml"
        )

    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Load the base class:
        name = ada.adaCamelCase(self.model_name) + "_Packets"
        self.load_suite_data(
            suite_name=name,
            suite_data=self.data,
            entities_name="packets",
            type_name="type",
            variable_types_allowed=True,
        )

        # If the ids for the packets have all been defined then sort the packets by id:
        if len(self.ids) > 0:
            self.entities = OrderedDict(
                sorted(self.entities.items(), key=lambda x: x[1].id)
            )

        # Add some packet specific data to the entity objects:
        for key, entity in self.entities.items():
            self.entities[key] = packet.from_ided_entity(entity)

        # Rename entities to something more descriptive:
        self.packets = list(self.entities.values())

        # See if there are any packets without a type:
        self.typeless_packet = False
        for p in self.packets:
            if not p.type:
                self.typeless_packet = True
                break

    # Override this method. If we have a packet suite with static ids, then we do not want to call the base class,
    # otherwise we do.
    def set_id_base(self, start_id):
        if not self.ids:
            ided_suite.set_id_base(self, start_id)

    def set_component(self, component):
        # Set the id bases parameter:
        if not self.ids:
            component.set_id_bases_parameters.append(
                parameter(
                    name="packet_Id_Base",
                    type="Packet_Types.Packet_Id_Base",
                    description="The value at which the component's unresolved packet identifiers begin.",
                )
            )

        # Store all includes for the component:
        component.base_ads_includes.append(self.name)
        if not self.ids:
            component.base_ads_includes.append("Packet_Types")
        component.tester_base_ads_includes.extend(["Packet", self.name] + self.includes)
        if self.basic_types:
            component.tester_base_adb_includes.append("Serializer")
        if self.variable_length_types:
            component.tester_base_adb_includes.extend(
                ["Serializer_Types", "Basic_Types", "Byte_Array_Util"]
            )
        if self.typeless_packet:
            component.tester_base_adb_includes.append("Basic_Types.Representation")
        component.tester_base_adb_includes.extend(self.representation_includes)
        component.tester_template_ads_includes.extend(
            ["Printable_History", "Packet.Representation"]
            + self.representation_includes
        )

        # Do base class load:
        component_submodel.set_component(self, component)

        for key, entity in self.entities.items():
            entity.component = self.component
        self.packets = list(self.entities.values())

    def get_dependencies(self):
        return component_submodel.get_dependencies(self) + \
               ided_suite.get_dependencies(self)
