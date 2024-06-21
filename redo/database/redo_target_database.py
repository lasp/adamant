from database.database import database
from database.database import DATABASE_MODE
from database import util


class redo_target_database(database):
    """
    This database is responsible for storing "redo targets" which are
    potential files that the redo build system can build. The database
    structure looks like this:

    Key:                  Value:
    /path/to/directory : [/path/to/thing_to_build1.txt, \
    /path/to/directory/build/src/src_file.ads, \
    /path/to/directory/build/obj/src_file.o, etc.]

    The example above shows that in the directory called "directory" you can
    build many things, including a file called "build/src/src_file.ads".
    """
    def __init__(self, mode=DATABASE_MODE.READ_ONLY):
        """Initialize the database."""
        super(redo_target_database, self).__init__(
            util.get_database_file("redo_target"), mode
        )

    def get_targets_for_directory(self, directory):
        """Get the list of all targets associated with a given directory."""
        return list(self.fetch(directory))
