import sys
from util import ada

# This python utility is meant to be run from the command
# line. Given an ada specification file (.ads), this utility searches
# through it and determines if it would be wise to depend on the body (.adb) file
# as well. True or False is printed.
if __name__ == "__main__":
    if len(sys.argv) == 2:
        filename = sys.argv[1]
        print(ada.should_depend_on_adb(filename))
    else:
        print("Usage: should_depend_on_adb.py file.ads")
        sys.exit(1)
    sys.exit(0)
