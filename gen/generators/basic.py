from base_classes.generator_base import generator_base
from util import redo_arg
from util import html
from util import ada
import os.path


# Special helper function which allows you to dynamically create a basic generator at
# runtime. You can specify the module that you would like the generator to be part of.
def create_basic_generator(
    model_class,
    template_name,
    module_name=__name__,
    template_dir=None,
    has_dependencies=True,
    camel_case_filename=False,
    ignore_cache=False
):
    #
    # Dynamically generate a class of the form:
    #
    #  class packets_ads(basic_generator, generator_base):
    #    def __init__(self):
    #      basic_generator.__init__(self, model_class=packets.packets, template_filename="name_packets.ads")
    #

    def init_func(self):
        basic_generator.__init__(
            self,
            model_class=model_class,
            template_filename=template_name,
            template_dir=template_dir,
            has_dependencies=has_dependencies,
            camel_case_filename=camel_case_filename,
            ignore_cache=ignore_cache
        )

    cls_name = (
        model_class.__name__
        + "__"
        + template_name.replace("name_", "").replace("-", "_").replace(".", "_")
    )
    return type(
        cls_name,
        (
            basic_generator,
            generator_base,
        ),
        {"__init__": init_func, "__module__": module_name},
    )


# Add a class to a specific module. By default, calling this will add the generator to
# the module you called the function from.
def add_class_to_module(cls, module=globals()):
    module[cls.__name__] = cls


# Add a basic generator to a specific module:
def add_basic_generator_to_module(
    model_class,
    template,
    module=globals(),
    template_dir=None,
    has_dependencies=True,
    camel_case_filename=False,
    ignore_cache=False
):
    module_name = module["__name__"]
    cls = create_basic_generator(
        model_class,
        template,
        module_name=module_name,
        template_dir=template_dir,
        has_dependencies=has_dependencies,
        camel_case_filename=camel_case_filename,
        ignore_cache=ignore_cache
    )
    add_class_to_module(cls, module)


# Add a list of basic generators to specific module, where each generator has a different output template.
def add_basic_generators_to_module(
    model_class,
    templates,
    module=globals(),
    template_dir=None,
    has_dependencies=True,
    camel_case_filename=False,
    ignore_cache=False
):
    for template in templates:
        add_basic_generator_to_module(
            model_class,
            template,
            module=module,
            template_dir=template_dir,
            has_dependencies=has_dependencies,
            camel_case_filename=camel_case_filename,
            ignore_cache=ignore_cache
        )


#
# This package contains a basic generator which implements a generator pattern
# common for most Adamant generators. Generators should create a child class inheriting from
# both this class and generator_base. ie. something like this:
#
#   class new_generator(basic_generator, generator_base):
#     def __init__(self):
#       basic_generator.__init__(self, model_class=new_model.new_model, template_filename="name_cool_file.ads")
#
# where model_class is the python class that contains the generator model and template_filename contains
# the filename of the template used to render the output file.
#
# Most generators will work as expected if the follow the Adamant generator pattern. Besides implementing
# the generator as shown above, the template file name should exactly mimic the format of the output
# filename with the string "name" substituted in for the model name that generates the output. For example
# if the model name is something like awesome.component.yaml then the following template names will
# result in the shown output names:
#
#   name.html -> awesome.html
#   whatever.ads -> whatever.ads
#   component-name-implementation.adb -> component-awesome-implementation.adb
#
# Output files will be constructed relative to the input model file in their default adamant directories.
# The rules are outlined below:
#
#   files extension ".tex" -> doc/build/tex
#   files extension ".ad[s,b]" -> build/src
#   files ending in "[main, test, -implementation, -implementation-tester].ad[s,b]" -> build/template
#   all others -> build/<file extension>
#
# If this behavior doesn't fit your generator you have two choices:
#
#   1) Override the methods of basic_generator where you need different behavior
#   2) Write your generator from scratch inheriting from generator_base
#
class basic_generator(object):
    def __init__(
        self,
        model_class,
        template_filename,
        template_dir=None,
        has_dependencies=True,
        camel_case_filename=False,
        ignore_cache=False
    ):
        # Set the generator model class and type name:
        self.model_cls = model_class
        self._model_obj = {}
        self.model_type = self.model_cls.__name__.lower()
        self.has_dependencies = has_dependencies
        self.camel_case_filename = camel_case_filename
        self.ignore_cache = ignore_cache

        # If a full template filename is not given, then form one using
        # the model_type:
        self.template_dir = template_dir
        self.template = template_filename.strip()
        self.template_basename = os.path.basename(self.template)
        if self.template == self.template_basename:
            self.template = os.sep + self.model_type + os.sep + self.template_basename

        # Set the extension and descriptor:
        name, ext = os.path.splitext(self.template_basename)
        self.extension = ext[1:]
        self.descriptor = name.split("name")[-1]

    # Cache model object for speed:
    def model_object(self, input_filename):
        if input_filename not in self._model_obj:
            self._model_obj[input_filename] = self.model_cls(input_filename, ignore_cache=self.ignore_cache)
        return self._model_obj[input_filename]

    def input_file_regex(self):
        return r".*\." + self.model_type + r"\.yaml$"

    # Extracts useful info from the input filename. Examples:
    #
    # /path/to/my_component.component.yaml -> (/path/to, None, my_component, component, yaml)
    # /path/to/my_test.my_component.tests.yaml -> (/path/to, my_test, my_component, tests, yaml)
    #
    def _split_input_filename(self, input_filename):
        return redo_arg.split_model_filename(input_filename, self.model_type)

    # Return the default build directory for this generator based on the
    # template's file extension and name.
    def _get_default_build_dir(self):
        # Set defaults:
        build_dir = "build"
        sub_dir = self.extension

        # Add doc directory if the output is tex:
        if self.model_type in ["component", "assembly"] and self.extension == "tex":
            build_dir = "doc" + os.sep + build_dir
            # If there is no descriptor, then this needs to generated in template dir:
            if not self.descriptor:
                sub_dir = "template"

        # Use adamant defaults to determine the build subdirectory location:
        if self.extension in ["adb", "ads"]:
            if (
                self.descriptor.endswith("-implementation")
                or self.descriptor.endswith("-implementation-tester")
                or self.descriptor in ["main", "test"]
            ):
                sub_dir = "template"
            else:
                sub_dir = "src"

        return build_dir + os.sep + sub_dir

    # The default output filename is the name of the template with last instance of the string "name"
    # replaced with the name of the specific model we are using as input file.
    def _get_default_output_filename(self, model_name):
        a = self.template_basename.rsplit("name", maxsplit=1)
        name = model_name.join(a)
        if self.camel_case_filename:
            split_name = name.split(".")
            for idx, sn in enumerate(split_name[:-1]):
                split_name[idx] = ada.adaCamelCase(sn)
            name = ".".join(split_name)
        return name

    # Override this if it does not work for your specific generator. This function basically
    # uses the basic generator defaults for output file location and name.
    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, model_name, *ignore = self._split_input_filename(
            input_filename
        )

        # If the input filename is itself an autogenerated file in build, then reset the dir to be
        # the source dir of the autogenerated file.
        dir1, dir2, dir3 = redo_arg._get_3_dirs(input_filename)
        if dir2 == "build" or dir3 == "build":
            dirname = redo_arg.get_src_dir(input_filename)

        # Get the expected build directory:
        build_dir = self._get_default_build_dir()

        # Construct the filename:
        if specific_name:
            output_filename = self._get_default_output_filename(specific_name)
        else:
            output_filename = self._get_default_output_filename(model_name)

        # Return the suggested output filename:
        return dirname + os.sep + build_dir + os.sep + output_filename

    def _generate_output(self, input_filename, methods_to_call_on_model_obj=[]):
        model_obj = self.model_object(input_filename)

        # Call any desired methods on the model object. This performance feature is used to
        # add functionality to some models that are needed by only some generators,
        # while most generators can ignore the functionality.
        for method in methods_to_call_on_model_obj:
            getattr(model_obj, method)()

        output = model_obj.render(self.template, self.template_dir)

        # Handling for specific output file types:
        if self.extension == "html":
            # Search output for html dependencies and depend on them:
            output_filename = self.output_filename(input_filename)
            html.depend_on_html_links(output_filename, output)

        return output

    def generate(self, input_filename, methods_to_call_on_model_obj=[]):
        # Print the output:
        print(
            self._generate_output(
                input_filename,
                methods_to_call_on_model_obj=methods_to_call_on_model_obj,
            )
        )

    # Depend on the primary model dependencies:
    def depends_on(self, input_filename):
        if self.has_dependencies:
            m = self.model_object(input_filename)
            if hasattr(m, "get_dependencies"):
                # import sys
                # sys.stderr.write("depending on: " + str(m.get_dependencies()) + "\n")
                return m.get_dependencies()
        return []
