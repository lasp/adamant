from base_classes.generator_base import generator_base
from generators.basic import add_basic_generators_to_module
from generators.basic import basic_generator
from util import model_loader
from util import error
from models import assembly
from models import view
import os.path

# This module contains generators which produce source code,
# html, xml, and graphics files for an assembly. Each
# generator takes an assembly yaml file as input.

assembly_templates = [
    "assembly/name.ads",
    "assembly/name.adb",
    "assembly/name.dot",
    "assembly/name_commands.ads",
    "assembly/name_data_products.ads",
    "assembly/name_data_dependencies.ads",
    "assembly/name_events.ads",
    "assembly/name_parameters.ads",
    "assembly/name_packets.ads",
    "assembly/name_faults.ads",
    "assembly/name_events.py",
    "assembly/name_events.m",
    "assembly/name_commands.html",
    "assembly/name_events.html",
    "assembly/name_parameters.html",
    "assembly/name_data_products.html",
    "assembly/name_data_dependencies.html",
    # "assembly/name_packets.html",
    "assembly/name_faults.html",
    "assembly/name_components.html",
    "assembly/name_connections.html",
    "assembly/name_arrayed_connections.html",
    "assembly/name_priorities.html",
    "assembly/name_interrupts.html",
    "assembly/name_queues.html",
    "assembly/name.tex",
    "assembly/name_components.tex",
    "assembly/name_commands.tex",
    "assembly/name_connections.tex",
    "assembly/name_description.tex",
    "assembly/name_events.tex",
    "assembly/name_data_products.tex",
    "assembly/name_data_dependencies.tex",
    "assembly/name_parameters.tex",
    "assembly/name_packets.tex",
    "assembly/name_faults.tex",
    "assembly/name_stats.tex",
    "assembly/name_types.tex",
    "assembly/name_enums.tex",
    "assembly/name_priorities.tex",
]

this_module = globals()
add_basic_generators_to_module(
    assembly.assembly, assembly_templates, module=this_module
)

####################################################
# Base class for assembly generators:
####################################################


class assembly_generator(basic_generator):
    def __init__(self, template, template_dir=None):
        basic_generator.__init__(
            self,
            model_class=assembly.assembly,
            template_filename=template,
            template_dir=template_dir,
        )


class assembly_hydra_generator(assembly_generator):
    def __init__(self, template, template_dir=None, subdir=None):
        assembly_generator.__init__(self, template=template, template_dir=template_dir)
        self._subdir = subdir

    def _get_default_build_dir(self):
        if self._subdir:
            return "build" + os.sep + "hydra" + os.sep + self._subdir
        else:
            return "build" + os.sep + "hydra" + os.sep + self.extension


####################################################
# Assembly generators:
####################################################


class assembly_views_tex(assembly_generator, generator_base):
    def __init__(self):
        assembly_generator.__init__(self, "name_views.tex")

    def generate(self, input_filename, methods_to_call_on_model_obj=[]):
        assembly_generator.generate(
            self,
            input_filename,
            methods_to_call_on_model_obj=["load_view_models"]
            + methods_to_call_on_model_obj,
        )


class hydra_packet_pages_prc(assembly_hydra_generator, generator_base):
    def __init__(self):
        assembly_hydra_generator.__init__(
            self, "name_packet_pages.prc", subdir="Scripts"
        )


##############################################
# Hydra XML generator:
##############################################
# Mapping from basic Ada types to format strings for Hydra.
type_format_dictionary = {"U": "%d", "I": "%d", "F": "%f"}


# Function which produces a format string to print an Ada type:
def create_type_print_strings(theAssembly):
    for name, model in theAssembly.complex_types.items():
        string = ""
        model_type = type(model).__name__
        if model_type in ["record", "array"]:
            for field_name, field in model.fields.items():
                string += field_name + " = "
                if field.is_enum:
                    string += (
                        "$"
                        + field.type_model.suite.name
                        + "-"
                        + field.type_model.name
                        + "_Dict(%d)"
                    )
                elif field.format:
                    try:
                        string += type_format_dictionary[field.format.type[0]]
                    except Exception:
                        string += "%d"
                else:
                    string += (
                        "("
                        + theAssembly.complex_types[field.type_package].format_string
                        + ")"
                    )
                string += ", "
            # Remove the last comma and end the format string:
            string = string[:-2]
            model.format_string = string
        else:
            assert False, "Cannot handle models of type: " + str(model_type)


# Function which produces a type field string for all types
# in the assembly mode.
def create_type_field_strings(theAssembly):
    def form_field_strings(top_model, model, prefix=""):
        model_type = type(model).__name__
        if model_type in ["record", "array"]:
            for field_name, field in model.fields.items():
                try:
                    if field.is_packed_type:
                        form_field_strings(
                            top_model,
                            theAssembly.complex_types[field.type_package],
                            prefix + "-" + field_name,
                        )
                    else:
                        raise Exception()
                except Exception:
                    top_model.hydra_field_strings.append(
                        [
                            prefix + "-" + field_name,
                            field.format.type,
                            field.format.length,
                        ]
                    )
        else:
            assert False, "Cannot handle models of type: " + str(model_type)

    for name, model in theAssembly.complex_types.items():
        model.hydra_field_strings = []
        form_field_strings(model, model)


class assembly_xml(assembly_hydra_generator, generator_base):
    def __init__(self):
        assembly_hydra_generator.__init__(self, "name.xml", subdir="Config")

    def generate(self, input_filename):
        a = self.model_cls(input_filename)
        a.type_format_dictionary = type_format_dictionary
        create_type_print_strings(a)
        create_type_field_strings(a)
        print(a.render(self.template))


#############################################
# View generator:
#############################################


class assembly_view_dot(basic_generator, generator_base):
    def __init__(self):
        basic_generator.__init__(
            self, model_class=view.view, template_filename="/assembly/name.dot"
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        build_dir = self._get_default_build_dir()

        # Construct the filename:
        output_filename = assembly_name + "_" + view_name + ".dot"

        # Return the suggested output filename:
        return dirname + os.sep + build_dir + os.sep + output_filename

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
        v = view.view(input_filename)
        a = v.apply(a)
        print(a.render(self.template))

    # Depend on any commands, data products, events models that this component uses:
    def depends_on(self, input_filename):
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
        return a.get_dependencies() + [assembly_model_name]


####################################################
# Special flattened packet generatory:
####################################################


class assembly_flattened_packet_html(assembly_generator, generator_base):
    def __init__(self):
        assembly_generator.__init__(self, "name_packets.html")

    def generate(self, input_filename):
        #
        # Creating a little generator to use for each packet
        #
        from models import packets

        class _flattened_packet_html(basic_generator, generator_base):
            def __init__(self, packet_name, template_dir=None):
                self.packet_name = packet_name
                basic_generator.__init__(
                    self,
                    model_class=packets.packets,
                    template_filename="name_packet_flattened.html",
                    template_dir=template_dir,
                )

            def output_filename(self, input_filename):
                return out_dir_temp + os.sep + self.packet_name.lower() + ".html"

            def _generate_output(self, input_filename, packet_model):
                output = packet_model.render(self.template, self.template_dir)

                # Search output for html dependencies and depend on them:
                # output_filename = self.output_filename(input_filename)
                # html.depend_on_html_links(output_filename, output)
                # ^ There are no dependencies in these htmls so I am commenting
                #  this out

                # Print the output:
                return output

        # Generate the packet root html:
        model_obj = self.model_cls(input_filename)
        output = model_obj.render(self.template, self.template_dir)

        # Depend only on links that are not in the flattened packet directory, since
        # those will be build manually (outside of redo). See below.
        from util import redo
        from util import html

        output_file = self.output_filename(input_filename)
        html_links = html.get_html_links(output_file, output)
        links_to_depend_on = [
            link
            for link in html_links
            if os.path.basename(os.path.dirname(link)) == "html"
        ]
        redo.redo_ifchange(links_to_depend_on)

        # Render the output:
        print(output)

        #
        # Generate a detailed HTML for each packet that shows the flattened view
        #

        # First create temporary directory to store all the html
        from util import filesystem
        import shutil

        dirname, view_name, assembly_name, *ignore = self._split_input_filename(
            input_filename
        )
        html_dir = os.path.dirname(output_file)
        dirname = assembly_name + "_packets"
        out_dir = html_dir + os.sep + dirname
        out_dir_temp = os.environ["SESSION_TMP_DIR"] + os.sep + dirname
        filesystem.safe_makedir(out_dir_temp)

        # Create an HTML for each packet:
        a = self.model_cls(input_filename)
        for id, p in a.packets.items():
            fpm = _flattened_packet_html(p.name)
            with open(fpm.output_filename(input_filename), "w") as ofile:
                ofile.write(fpm._generate_output(input_filename, p))

        # Remove old directory and copy temp to the destination:
        if os.path.exists(out_dir):
            shutil.rmtree(out_dir)
        shutil.move(out_dir_temp, out_dir)
