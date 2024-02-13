from base_classes.generator_base import generator_base
from generators.basic import basic_generator
from models import tests, component
from util import error
from util import model_loader
import os.path

# This module contains generators which produce products for
# unit testing including Ada source code and html. The source
# code follows the AUnit unit test framework testing pattern.

####################################################
# Base class for unit test generators:
####################################################


class tests_generator(basic_generator):
    def __init__(self, template):
        basic_generator.__init__(
            self, model_class=tests.tests, template_filename=template
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, component_name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (component_name + "_tests").join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        basic_generator.generate(
            self, input_filename, methods_to_call_on_model_obj=["load_component"]
        )


####################################################
# Unit test generators:
####################################################


class tests_base_ads(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name.ads")


class tests_base_adb(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name.adb")


class tests_implementation_ads(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name-implementation.ads")


class tests_implementation_adb(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name-implementation.adb")


class tests_main_adb(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "test.adb")


class tests_suite_ads(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name-implementation-suite.ads")


class tests_suite_adb(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name-implementation-suite.adb")


class tests_html(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name.html")


class tests_tex(tests_generator, generator_base):
    def __init__(self):
        tests_generator.__init__(self, "name.tex")


####################################################
# Base class for component unit test generators:
####################################################


class tester_generator(basic_generator):
    def __init__(self, template):
        basic_generator.__init__(
            self, model_class=tests.tests, template_filename=template
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, component_name, *ignore = self._split_input_filename(
            input_filename
        )
        component_model_path = model_loader.get_model_file_path(
            component_name, model_types="component"
        )
        # If a component model path was found, then we know that this unit test is intended
        # to test a component. Otherwise, it is a basic package unit test, and we don't
        # want to generate any component tester output file
        if component_model_path:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = component_name.join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return ""

    def generate(self, input_filename):
        dirname, specific_name, component_name, *ignore = self._split_input_filename(
            input_filename
        )
        component_model_path = model_loader.get_model_file_path(
            component_name, model_types="component"
        )
        if component_model_path:
            # Override the model class. We are going to use the component model
            # to generate this output instead of the test model, since it contains
            # all the info needed.
            self.model_cls = component.component
            basic_generator.generate(self, component_model_path)

    # Depend on the component model and any commands, data products, events models that the component uses:
    def depends_on(self, input_filename):
        # Extract the name of the component from model file:
        dirname, specific_name, component_name, *ignore = self._split_input_filename(
            input_filename
        )
        if component_name:
            component_model = model_loader.try_load_model_by_name(
                component_name, model_types="component"
            )
            if not component_model:
                error.error_abort(
                    "Could not find a model file for component '"
                    + component_name
                    + "'."
                )
            return component_model.models_dependent_on + [component_model.full_filename]
        return []


####################################################
# Component unit test generators:
####################################################


class tester_component_base_ads(tester_generator, generator_base):
    def __init__(self):
        tester_generator.__init__(self, "component-name_reciprocal.ads")


class tester_component_base_adb(tester_generator, generator_base):
    def __init__(self):
        tester_generator.__init__(self, "component-name_reciprocal.adb")


class tester_component_implementation_ads(tester_generator, generator_base):
    def __init__(self):
        tester_generator.__init__(self, "component-name-implementation-tester.ads")


class tester_component_implementation_adb(tester_generator, generator_base):
    def __init__(self):
        tester_generator.__init__(self, "component-name-implementation-tester.adb")
