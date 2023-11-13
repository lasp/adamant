from generators.basic import add_basic_generators_to_module
from models import configuration

configuration_templates = ["configuration/configuration.ads"]

this_module = globals()
add_basic_generators_to_module(
    configuration.configuration, configuration_templates, module=this_module
)
