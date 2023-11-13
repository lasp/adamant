import sys
from util import ada

# This python utility is meant to be run from the command
# line. Given an ada source file, this utility searches
# through it and finds all "with" dependencies (as well
# as other assumed dependencies) and prints each dependency
# on a separate line to stdout.
if __name__ == "__main__":
    if len(sys.argv) == 2:
        filename = sys.argv[1]
        for dependency in ada.get_source_dependencies(filename):
            print(dependency)
    else:
        print("Usage: get_ada_dependencies.py file.ads")
        sys.exit(1)
    sys.exit(0)
