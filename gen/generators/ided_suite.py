from generators.basic import add_basic_generators_to_module
from models import commands
from models import events
from models import data_products
from models import data_dependencies
from models import parameters
from models import packets
from models import faults
from models import requirements

# This module contains generators which produce products related
# to a ided suites including Ada source code and html.

# List out all the templates used for each ided entity suite. We do this
# so that submodels that want to override these suites can easily get a list
# of all the standard templates, and use them to dynamically generate
# their own special generators. See the cpu monitor component generators for
# a good example of this.
command_templates = [
    "commands/name_commands.ads",
    "commands/name_commands.adb",
    "commands/name_commands.html",
]
parameter_templates = [
    "parameters/name_parameters.ads",
    "parameters/name_parameters.adb",
    "parameters/name_parameters.html",
]
event_templates = [
    "events/name_events.ads",
    "events/name_events.adb",
    "events/name_events-representation.ads",
    "events/name_events-representation.adb",
    "events/name_events.html",
]
data_product_templates = [
    "data_products/name_data_products.ads",
    "data_products/name_data_products.adb",
    "data_products/name_data_products-representation.ads",
    "data_products/name_data_products-representation.adb",
    "data_products/name_data_products.html",
]
data_dependency_templates = [
    "data_dependencies/name_data_dependencies.ads",
    "data_dependencies/name_data_dependencies.adb",
    "data_dependencies/name_data_dependencies.html",
]
packet_templates = [
    "packets/name_packets.ads",
    "packets/name_packets.adb",
    "packets/name_packets-representation.ads",
    "packets/name_packets-representation.adb",
    "packets/name_packets.html",
]
fault_templates = [
    "faults/name_faults.ads",
    "faults/name_faults.adb",
    "faults/name_faults.html",
]
requirement_templates = ["requirements/name_requirements.html"]

# Dynamically add the generators to this module.
this_module = globals()
add_basic_generators_to_module(commands.commands, command_templates, module=this_module)
add_basic_generators_to_module(
    parameters.parameters, parameter_templates, module=this_module
)
add_basic_generators_to_module(events.events, event_templates, module=this_module)
add_basic_generators_to_module(
    data_products.data_products, data_product_templates, module=this_module
)
add_basic_generators_to_module(
    data_dependencies.data_dependencies, data_dependency_templates, module=this_module
)
add_basic_generators_to_module(packets.packets, packet_templates, module=this_module)
add_basic_generators_to_module(faults.faults, fault_templates, module=this_module)
add_basic_generators_to_module(
    requirements.requirements, requirement_templates, module=this_module
)
