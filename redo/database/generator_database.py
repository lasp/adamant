from database.database import database
from database.database import DATABASE_MODE
from database import util


class generator_database(database):
    """
    This database is responsible for storing information for all the
    "generators" in the system, which are objects inheriting from
    generator_base that can generate text files for the system.
    The database maps each output file that the generator system can produce
    to a tuple which includes information about the generator and input
    file combination that can produce that particular output file.
    An example is shown below.

    Key:                          Value:
    /path/to/generated/file.txt : (generator_module_name, \
    /path/to/generator_module.py, \
    generator_class_name, \
    /path/to/input_file_name.txt)

    Using information in this database the build system can quickly
    figure out how to generate a particular output file when asked
    by the user.
    """
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(generator_database, self).__init__(
            util.get_database_file("generator"), mode
        )

    def insert_generator(
        self,
        output_filename,
        input_filename,
        generator_module_name,
        generator_module_file_name,
        generator_class_name,
    ):
        """
        Insert generator and input file information into the database for
        a certain output file. Note that only a single entry is allowed
        for each output file. If a second entry for an output file is
        detected, an error message will be thrown so that the user can
        resolve the problem.
        """
        to_store = (
            generator_module_name,
            generator_module_file_name,
            generator_class_name,
            input_filename,
        )

        def error_message():
            return (
                "Only ONE input file and generator can be registered to generate the outputfile: "
                + output_filename
                + "\n"
                + "    "
                + str(self.fetch(output_filename))
                + " is already registered.\n"
                + "    "
                + str(to_store)
                + " cannot be registered.\n"
            )

        assert not self.does_key_exist(output_filename), error_message()
        self.store(output_filename, to_store)

    def get_generator(self, output_filename):
        """
        Given an output filename, return the tuple of information stored
        in the database for that entry.
        """
        return self.fetch(output_filename)
