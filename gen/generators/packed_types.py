from generators.basic import add_basic_generators_to_module, basic_generator
from base_classes.generator_base import generator_base
from models import record
from models import array
from models import enums

record_templates = [
    "record/name.ads",
    "record/name.adb",
    "record/name-representation.ads",
    "record/name-representation.adb",
    "record/name-validation.ads",
    "record/name-validation.adb",
    "record/name-assertion.ads",
    "record/name-assertion.adb",
    "record/name-c.ads",
    "record/name.py",
    "record/name_type_ranges.adb",
]

array_templates = [
    "array/name.ads",
    "array/name.adb",
    "array/name-representation.ads",
    "array/name-representation.adb",
    "array/name-validation.ads",
    "array/name-validation.adb",
    "array/name-assertion.ads",
    "array/name.py",
    "array/name_type_ranges.adb",
]

enums_templates = [
    "enums/name.ads",
    "enums/name-representation.ads",
    "enums/name-representation.adb",
    "enums/name-assertion.ads",
    "enums/name.tex",
    "enums/name.html",
    "enums/name.py",
]

this_module = globals()
add_basic_generators_to_module(record.record, record_templates, module=this_module)
add_basic_generators_to_module(
    record.record, ["record/name.m"], module=this_module, camel_case_filename=True
)
add_basic_generators_to_module(array.array, array_templates, module=this_module)
add_basic_generators_to_module(
    array.array, ["array/name.m"], module=this_module, camel_case_filename=True
)
add_basic_generators_to_module(enums.enums, enums_templates, module=this_module)


class doc_generator(basic_generator):
    def generate(self, input_filename, methods_to_call_on_model_obj=[]):
        basic_generator.generate(
            self,
            input_filename,
            methods_to_call_on_model_obj=methods_to_call_on_model_obj
            + ["load_type_ranges"],
        )


class record_html(doc_generator, generator_base):
    def __init__(self):
        doc_generator.__init__(
            self, model_class=record.record, template_filename="name.html"
        )


class record_tex(doc_generator, generator_base):
    def __init__(self):
        doc_generator.__init__(
            self, model_class=record.record, template_filename="name.tex"
        )


class array_html(doc_generator, generator_base):
    def __init__(self):
        doc_generator.__init__(
            self, model_class=array.array, template_filename="name.html"
        )


class array_tex(doc_generator, generator_base):
    def __init__(self):
        doc_generator.__init__(
            self, model_class=array.array, template_filename="name.tex"
        )


# Special enum generator for matlab that splits the output into multiple files, one per class
# as required by matlab.
class enum_m_generator(basic_generator, generator_base):
    def __init__(self):
        basic_generator.__init__(
            self,
            model_class=enums.enums,
            template_filename="enums/name.m",
            camel_case_filename=True,
        )

    def output_filename(self, input_filename):
        return basic_generator.output_filename(self, input_filename)[:-2]

    def generate(self, input_filename, methods_to_call_on_model_obj=[]):
        #
        # This is a unique generator in that in takes a single input a name.enums.yaml
        # and produces multiple outputs. The target will be a directory in build/m/name
        # that contains a .m file for eacn enumeration defined in name.enums.yaml. The
        # reason for this is that matlab only allows a single class definition per file
        # unlike python or Ada, which allows multiple enum definitions per file.
        #
        output = self._generate_output(
            input_filename, methods_to_call_on_model_obj=methods_to_call_on_model_obj
        )
        split_output = output.split("%!%!-split-here!%!%")

        # Make output directory
        from util import filesystem
        import os

        output_dir = self.output_filename(input_filename)
        filesystem.safe_makedir(output_dir)

        # Output each spit output to a unique file:
        for out in split_output[1:]:
            split_split_out = out.split("%!%!-split-again-here!%!%")
            filename = split_split_out[0].strip()
            content = split_split_out[1].strip()

            # Write content to m file:
            with open(output_dir + os.sep + filename, "w") as f:
                f.write(content)
