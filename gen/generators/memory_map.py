from generators.basic import add_basic_generators_to_module
from generators.basic import basic_generator
from base_classes.generator_base import generator_base
from models import memory_map
from models import register_map
from util import redo_arg
from util import error
import sys

memory_map_templates = [
    "memory_map/name.html",
    "memory_map/name.tex",
]

register_map_templates = [
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


class map_ads_generator(basic_generator):
    """Base class for generating a memory/register map and running GNATprove on the result."""
    def __init__(self, model_class, template_filename):
        basic_generator.__init__(
            self, model_class=model_class, template_filename=template_filename
        )

    def generate(self, input_filename, methods_to_call_on_model_obj=[]):
        # Capture the output into a string:
        output_filename = self.output_filename(input_filename)
        base_dir = redo_arg.get_src_dir(input_filename)
        output = basic_generator._generate_output(self, input_filename)

        # Write the output to a temporary file so we can analyze it with SPARK
        # prior to outputting the file to the end user.
        import os
        from util import filesystem

        # Write output to temp file
        ret = 0
        tempdir = os.path.dirname(output_filename) + os.sep + "temp"
        filesystem.safe_makedir(tempdir)
        path = tempdir + os.sep + os.path.basename(output_filename)
        with open(path, "w") as tmp:
            tmp.write(output)

        # Analyze with GNATprove
        from database import setup
        from rules import build_prove

        #
        # Create build system sandbox. The GNATprove prover can only be run on the target Linux_Prove,
        # even if we are compiling for an embedded target. So we create a build system sandbox to
        # run the prover, and then destroy it after running the prover.
        #
        old_target = setup.create_sandbox(
            output_filename,
            os.path.splitext(output_filename)[0],
            output_filename,
            "Linux_Prove",
        )

        # Analyze the file with gnatprove to make sure the output does not violate
        # any SPARK analysis.
        ret = build_prove._prove_ada_sources([path], base_dir)

        #
        # OK, we no longer need the sandbox, so restore the previous build system
        # configuration.
        #
        setup.destroy_sandbox(
            output_filename,
            os.path.splitext(output_filename)[0],
            output_filename,
            old_target,
        )

        # Exit early if error occurred:
        if ret != 0:
            error.error_print(
                "Analyzing generated map " + output_filename + " with GNATprove failed."
            )
            error.error_print(
                "Please address any warnings used by fixing/removing any bit-constrained types used in the map."
            )
            error.error_print("See temporary generated map here: " + path)
            sys.exit(ret)

        # Now output the file to stdout:
        print(output)


class memory_map_ads_generator(map_ads_generator, generator_base):
    def __init__(self):
        map_ads_generator.__init__(
            self,
            model_class=memory_map.memory_map,
            template_filename="/memory_map/name.ads",
        )


class register_map_ads_generator(map_ads_generator, generator_base):
    def __init__(self):
        map_ads_generator.__init__(
            self,
            model_class=register_map.register_map,
            template_filename="/register_map/name.ads",
        )
