import os.path
from util import redo
from util import filesystem
from util import debug
from util import error
from util import meta
from database.generator_database import generator_database
from base_classes.build_rule_base import build_rule_base
from models.exceptions import ModelException
import sys


def _generate(output_filename):
    """
    This private function runs a generator. It first inspects
    the output filename and makes sure that it is a registered
    product in the generator database. If it is, it retrieves the
    appropriate generator, and runs it on the input file to
    generate the output file.
    """
    # Make sure the output_filename is an absolute path:
    assert os.path.isabs(output_filename), (
        "Output file names given to the generic generator must be absolute paths. The given path is not absolute: "
        + str(output_filename)
    )

    # Attach to the database and grab the data for the output target filename:
    try:
        with generator_database() as db:
            value = db.get_generator(output_filename)
    except KeyError:
        error.error_abort("No rule to build " + output_filename + ".")

    # Extract the generator module, class and filename
    # and the input filename from the data:
    module_name = value[0]
    class_name = value[1]
    file_name = value[2]
    input_filename = value[3]

    # Import the module and create an instance of the generator:
    module = meta.import_module_from_filename(file_name, module_name)
    generator_class = getattr(module, class_name)
    generator = generator_class()

    def error_print(generator, exception):
        error.error_abort(
            generator.__module__
            + "."
            + generator.generate.__self__.__class__.__name__
            + "."
            + generator.generate.__name__
            + "() failed to build "
            + output_filename
            + " from "
            + input_filename
            + " with error msg:\n"
            + str(exception)
        )

    # First depend on the input file and the generator, since we know these are dependencies. We also want to make sure
    # we build the input file, should it also be a generated file.
    redo.redo_ifchange([input_filename, sys.modules[generator.__module__].__file__])

    # Get the dependencies from the generator:
    debug.debug_do_print(
        lambda: generator.__module__
        + "."
        + generator.depends_on.__self__.__class__.__name__
        + "."
        + generator.depends_on.__name__
        + "("
        + input_filename
        + ")"
    )
    try:
        dependencies = generator.depends_on(input_filename)
    except ModelException as e:
        error_print(generator, e)

    # Depend on generator specific dependencies:
    if dependencies:
        if isinstance(dependencies, str):
            dependencies = [dependencies]
        redo.redo_ifchange(list(set(dependencies)))

    # Make sure the output directory for the file exists. If it doesn't, create it:
    dirname = os.path.dirname(output_filename)
    filesystem.safe_makedir(dirname)

    # Run the generator create the output file:
    debug.debug_do_print(
        lambda: generator.__module__
        + "."
        + generator.generate.__self__.__class__.__name__
        + "."
        + generator.generate.__name__
        + "("
        + input_filename
        + ") -> "
        + output_filename
    )
    try:
        generator.generate(input_filename)
    except ModelException as e:
        error_print(generator, e)


class build_via_generator(build_rule_base):
    """
    This build rule runs a generator class. It does not provide
    an input file regex, so should be called from a default.do
    file.
    """
    def _build(self, redo_1, redo_2, redo_3):
        _generate(redo_1)

    # No need to provide these for generators
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
