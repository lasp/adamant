# Database imports:
import sys
import os
from database.source_database import source_database
from database.c_source_database import c_source_database
from database.py_source_database import py_source_database
from database.generator_database import generator_database
from database.utility_database import utility_database
from database.redo_target_database import redo_target_database
from database.build_target_database import build_target_database
from database.model_cache_database import model_cache_database
from database.model_database import model_database

# This python utility is meant to be run from the command
# line. When run it opens all the databases associated with
# the build system and print their contents to stdout.
if __name__ == "__main__":
    args = sys.argv[-2:]
    if len(args) != 2 or args[-1].endswith("print_database.py"):
        print("usage:\n  print_database.py /path/to/session_dir")
        print("")
        print("Note: you can find the session dir by doing the following:")
        print(
            "1) Make sure Adamant does not clean up the session directory so you can inspect it"
        )
        print("$ export ADAMANT_DISABLE_SESSION_CLEANUP=1")
        print("2) Turn on debug mode so that the session directory gets printed out")
        print("$ export DEBUG=1")
        print("3) Run 'redo what' from your desired directory to rebuild the databases")
        print("$ redo what")
        print(
            "4) Look for something like this 'SESSION_TMP_DIR = /tmp/tmp.WD9riBXRik/909535' near the top of the output."
        )
        print(
            "5) Use the session directory you found in your print_database.py command"
        )
        print("$ python print_database.py /tmp/tmp.WD9riBXRik/909535")
        print("6) profit!")
        sys.exit()
    os.environ["SESSION_TMP_DIR"] = args[1]

    print("Source Database:")
    with source_database() as db:
        print(str(db))
    print("")
    print("C Source Database:")
    with c_source_database() as db:
        print(str(db))
    print("")
    print("Python Source Database:")
    with py_source_database() as db:
        print(str(db))
    print("")
    print("Generator Database:")
    with generator_database() as db:
        print(str(db))
    print("")
    print("Redo Target Database:")
    with redo_target_database() as db:
        print(str(db))
    print("")
    print("Build Target Database:")
    with build_target_database() as db:
        print(str(db))
    print("")
    print("Utility Database:")
    with utility_database() as db:
        print(str(db))
    print("")
    print("Model Cache Database:")
    with model_cache_database() as db:
        print(str(db))
    print("")
    print("Model Database:")
    with model_database() as db:
        print(str(db))

    del os.environ["SESSION_TMP_DIR"]
