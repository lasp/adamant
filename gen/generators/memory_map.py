from generators.basic import add_basic_generators_to_module
from models import memory_map
from models import register_map

memory_map_templates = [
    "memory_map/name.ads",
    "memory_map/name.html",
    "memory_map/name.tex",
]

register_map_templates = [
    "register_map/name.ads",
    "register_map/name.html",
    "register_map/name.tex",
]

this_module = globals()
add_basic_generators_to_module(
    memory_map.memory_map, memory_map_templates, module=this_module
)
add_basic_generators_to_module(
    register_map.register_map, register_map_templates, module=this_module
)
