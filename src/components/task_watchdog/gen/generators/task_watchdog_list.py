import os.path
from models import task_watchdog_list
from models import task_watchdog_faults
from models import task_watchdog_data_products
from models import task_watchdog_commands
from base_classes.generator_base import generator_base
from generators.basic import basic_generator, add_basic_generators_to_module
from generators.ided_suite import fault_templates
from generators.ided_suite import data_product_templates
from generators.ided_suite import command_templates

add_basic_generators_to_module(
    task_watchdog_faults.task_watchdog_faults, fault_templates, module=globals()
)
add_basic_generators_to_module(
    task_watchdog_data_products.task_watchdog_data_products,
    data_product_templates,
    module=globals(),
)
add_basic_generators_to_module(
    task_watchdog_commands.task_watchdog_commands, command_templates, module=globals()
)


class task_watchdog_list_ads(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=task_watchdog_list.task_watchdog_list,
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
            output_filename = (assembly_name + "_task_watchdog_list").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        w = task_watchdog_list.task_watchdog_list(input_filename)
        w.load_assembly()
        print(w.render(self.template, template_path=self.template_dir))


class task_watchdog_list_status_record_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=task_watchdog_list.task_watchdog_list,
            template_filename="name_state_record.record.yaml",
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
            output_filename = (assembly_name + "_task_watchdog_list").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        w = task_watchdog_list.task_watchdog_list(input_filename)
        print(w.render(self.template, template_path=self.template_dir))


class task_watchdog_limit_cmd_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=task_watchdog_list.task_watchdog_list,
            template_filename="name_watchdog_limit_cmd.record.yaml",
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
            output_filename = (assembly_name + "_task_watchdog_list").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        w = task_watchdog_list.task_watchdog_list(input_filename)
        w.load_assembly(shallow_load=True)
        print(w.render(self.template, template_path=self.template_dir))


class task_watchdog_action_cmd_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=task_watchdog_list.task_watchdog_list,
            template_filename="name_watchdog_action_cmd.record.yaml",
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
            output_filename = (assembly_name + "_task_watchdog_list").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        w = task_watchdog_list.task_watchdog_list(input_filename)
        w.load_assembly(shallow_load=True)
        print(w.render(self.template, template_path=self.template_dir))
