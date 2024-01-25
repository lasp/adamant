import os.path
from models import assembly
from models import fault_responses
from models import fault_correction_data_products
from models import fault_correction_commands
from base_classes.generator_base import generator_base
from util import error
from util import model_loader
from generators.basic import basic_generator, add_basic_generators_to_module
from generators.ided_suite import data_product_templates, command_templates

# Add all the basic generators for normal packets, except use the sequence_store_packets model.
# This allows us to build all the normal packet outputs like html, tex, etc.
add_basic_generators_to_module(
    fault_correction_data_products.fault_correction_data_products,
    data_product_templates,
    module=globals(),
)
add_basic_generators_to_module(
    fault_correction_commands.fault_correction_commands,
    command_templates,
    module=globals(),
)


class fault_responses_ads(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=fault_responses.fault_responses,
            template_filename="name.ads",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (assembly_name + "_fault_responses").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        assembly_model_name = model_loader.get_model_file_path(
            assembly_name, model_types="assembly"
        )
        if not assembly_model_name:
            error.error_print(
                "Could not find a model file for assembly '" + assembly_name + "'."
            )
            error.abort()
        a = assembly.assembly(assembly_model_name)
        r = fault_responses.fault_responses(input_filename)
        r.resolve_fault_and_command_ids(a)
        print(r.render(self.template, template_path=self.template_dir))


class fault_responses_status_record_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=fault_responses.fault_responses,
            template_filename="name_status_record.record.yaml",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (assembly_name + "_fault_responses").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        r = fault_responses.fault_responses(input_filename)
        print(r.render(self.template, template_path=self.template_dir))


class fault_responses_status_enum_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=fault_responses.fault_responses,
            template_filename="name_enums.enums.yaml",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (assembly_name + "_fault_responses").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        assembly_model_name = model_loader.get_model_file_path(
            assembly_name, model_types="assembly"
        )
        if not assembly_model_name:
            error.error_print(
                "Could not find a model file for assembly '" + assembly_name + "'."
            )
            error.abort()
        a = assembly.assembly(
            assembly_model_name, shallow_load_component_list=["Fault_Correction"]
        )
        r = fault_responses.fault_responses(input_filename)
        r.resolve_fault_and_command_ids(a)
        print(r.render(self.template, template_path=self.template_dir))


class fault_responses_packed_id_type_record_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=fault_responses.fault_responses,
            template_filename="name_packed_id_type.record.yaml",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (assembly_name + "_fault_responses").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        r = fault_responses.fault_responses(input_filename)
        print(r.render(self.template, template_path=self.template_dir))

    def depends_on(self, input_filename):
        deps = basic_generator.depends_on(self, input_filename)
        enum_gen = fault_responses_status_enum_yaml()
        deps.append(enum_gen.output_filename(input_filename))
        return deps
