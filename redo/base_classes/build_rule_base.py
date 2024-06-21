import abc
import os.path


class build_rule_base(metaclass=abc.ABCMeta):
    """
    This is the base object for build rules. All other
    build_rules should inherit from this base and implement the
    abstract methods. Build rules specify a build action to perform.
    Usually these actions are triggered by calling the "build"
    function of a build rule from within a .do file. This class
    construct is useful for "redo what" allowing it to
    collect ancillary data for each build rule, giving "redo
    what" insight into what products each build rule is able
    to produce.
    """
    @abc.abstractmethod
    def _build(self, redo_1, redo_2, redo_3):
        """
        Given the redo options, build the output file
        using this function. The build function below
        calls this private function.
        """
        pass

    def input_file_regex(self):
        r"""
        Optional: In order to aid "redo what" provide
        the input file extensions which would be valid
        to run this rule on. This won't pertain for
        everything, but for example if you have a rule
        that creates PDF files from any ".tex" file you
        would want to return ".*\.tex$" here in order to
        provide "redo what" with the option to build
        your ".pdf" file when a ".tex" file is found.
        """
        return None

    def output_filename(self, input_filename):
        """
        Optional: See input_file_extensions() above.
        This function returns the expected output_filename
        given an input filename found on the system. Again,
        this won't be feasible for all build rules, but if
        provided, "redo what" will be more accurate.
        """
        return None

    def build(self, redo_1, redo_2, redo_3):
        """
        Given the redo options, build the output file
        using this function.
        Optional, override this function if you
        do not need the database setup in order to
        execute your build rule. This is rare, but
        will cause your build rule to execute faster
        as a result.
        """
        # Get absolute paths for redo targets:
        redo_1 = os.path.abspath(redo_1)
        redo_2 = os.path.abspath(redo_2)
        redo_3 = os.path.abspath(redo_3)

        # Set up the build system:
        import database.setup

        did_setup = database.setup.setup(redo_1, redo_2, redo_3)

        # Call the abstract build method:
        to_return = self._build(redo_1, redo_2, redo_3)

        # Clean up the build system:
        if did_setup:
            database.setup.cleanup(redo_1, redo_2, redo_3)

        return to_return
