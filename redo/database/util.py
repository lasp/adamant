import os.path

# This module stores common function that all database classes use.


# Given the database name, return the location of its file.
def get_database_file(database_name):
    return os.path.join(
        os.path.join(os.environ["SESSION_TMP_DIR"], "db"), database_name + ".db"
    )
