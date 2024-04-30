from generators.basic import add_basic_generators_to_module, add_basic_generator_to_module
from models import component

# This module contains generators that produce products related
# to components including Ada source code, and graphics. Each
# generator takes a component yaml file as input.

component_templates = [
    "component/component-name.ads",
    "component/component-name.adb",
    "component/component-name-implementation.ads",
    "component/component-name-implementation.adb",
    "component/name_commands.tex",
    "component/name_connectors.tex",
    "component/name_description.tex",
    "component/name_events.tex",
    "component/name_data_products.tex",
    "component/name_data_dependencies.tex",
    "component/name_parameters.tex",
    "component/name_packets.tex",
    "component/name_faults.tex",
    "component/name_stats.tex",
    "component/name_init.tex",
    "component/name_preamble.tex",
    "component/name_types.tex",
    "component/name_enums.tex",
    "component/name_interrupts.tex",
    "component/name_requirements.tex",
]

component_templates_no_deps = ["component/name.dot", "component/name.tex"]

this_module = globals()
add_basic_generators_to_module(
    component.component, component_templates, module=this_module
)
add_basic_generators_to_module(
    component.component,
    component_templates_no_deps,
    module=this_module,
    has_dependencies=False,
)
# We cannot use the cached component model for anything to do with unit
# tests, since unit tests are not in the path, and need to be found by
# searching the filesystem each time. By doing this, we will always detect
# any new unit tests models that might appear on the filesystem for a
# component.
add_basic_generator_to_module(
    component.component,
    "component/name_unit_test.tex",
    module=this_module,
    ignore_cache=True
)
