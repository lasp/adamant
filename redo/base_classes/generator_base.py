import abc
from util import redo_arg
import os.path


class generator_base(metaclass=abc.ABCMeta):
    """
    This is the base object for generators. All other
    generators should inherit from this base and implement the
    abstract methods. Generators generate a textual product
    based on some input file ie. generating html from a yaml
    model file.
    """
    @abc.abstractmethod
    def input_file_regex(self):
        r"""
        A regex that matches the input filetype for
        this generator. ie. if you want to match files
        that end in *.component.yaml you would return
        the regex ".*\.component\.yaml$"
        """
        pass

    @abc.abstractmethod
    def output_filename(self, input_filename):
        """
        A function that, given an input filename, returns
        the corresponding output filename. ie. maybe
        the input filename is component_name.component.yaml
        you might write a function that returns
        component_name.html, if your generator produces html
        files. Note that you should return an absolute path
        to the output filename. You can assume that the input
        filename is always an absolute path.
        """
        pass

    def output_filename_(self, input_filename):
        """
        This function calls the output_filename function above
        and is the actual function called by the build system in
        order to construct build rules based on this generator.
        The purpose of this function is to make the default behavior
        to only create a rule for a generated file if an overriding
        file does not exist. A generated source file (something constructed
        in a "build/src" directory) can be overridden if a file is found
        with the same name above that build directory. In that case
        this function will suppress the generation of the generator
        build rule so that the hand written "overriding" file is
        used instead.

        Note: this function can be overridden by the inheriting class
        if this default behavior is not desired.
        """
        # Call the inherited class output_filename method:
        output = self.output_filename(input_filename)

        # Make sure the file is in a build/src directory:
        if output and redo_arg.in_build_src_dir(output):
            # If an overriding file exists, return nothing.
            dirname, basename = redo_arg.split_redo_arg(output)
            base_dir = os.path.dirname(os.path.dirname(dirname))
            if os.path.isfile(base_dir + os.sep + basename):
                return ""
        return output

    @abc.abstractmethod
    def generate(self, input_filename):
        """
        Generate the output file give the input file.
        Currently, the correct way to do this is to output
        the contents of your output file on stdout. The
        build system will take anything that you "print()"
        on stdout and store it in the output file using
        redo.
        """
        pass

    def depends_on(self, input_filename):
        """
        Optional: Provide list of dependencies for the generator.
        Note that the input_filename as well as the
        module file that the generator itself is contained in
        are implicit dependencies and will be tracked. They do
        not need to be provided by this function:
        """
        return []
