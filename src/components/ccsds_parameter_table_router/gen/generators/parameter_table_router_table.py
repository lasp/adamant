import os.path
from models import assembly
from models import parameter_table_router_table
from base_classes.generator_base import generator_base
from util import error
from util import model_loader
from generators.basic import basic_generator


class _parameter_table_router_table_base(basic_generator):
    """
    Shared output_filename / generate behavior for the router_table
    generators below.

    Intentionally NOT a generator_base subclass: redo's generator auto-
    discovery (redo/database/create.py) iterates every generator_base
    subclass and instantiates each with no args. This template-less
    base requires a template_filename and would explode there. The
    concrete subclasses below inherit from generator_base directly and
    pass the hardcoded template_filename through super().__init__().
    """
    def __init__(self, template_filename):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=parameter_table_router_table.parameter_table_router_table,
            template_filename=template_filename,
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        dirname, specific_name, component_name, *ignore = (
            self._split_input_filename(input_filename)
        )
        if not specific_name:
            build_dir = self._get_default_build_dir()
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (
                component_name + "_parameter_table_router_table"
            ).join(a)
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        dirname, view_name, assembly_name, *ignore = (
            self._split_input_filename(input_filename)
        )
        assembly_model_name = model_loader.get_model_file_path(
            assembly_name, model_types="assembly"
        )
        if not assembly_model_name:
            error.error_print(
                "Could not find a model file for assembly '"
                + assembly_name
                + "'."
            )
            error.abort()
        a = assembly.assembly(assembly_model_name)
        r = parameter_table_router_table.parameter_table_router_table(
            input_filename
        )
        r.resolve_router_destinations(a)
        print(r.render(self.template, template_path=self.template_dir))


class parameter_table_router_table_ads(_parameter_table_router_table_base, generator_base):
    def __init__(self):
        super().__init__(template_filename="name.ads")


class parameter_table_router_table_py(_parameter_table_router_table_base, generator_base):
    def __init__(self):
        super().__init__(template_filename="name.py")
