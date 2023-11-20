from util import binary_meta
import sys

# This python utility is meant to be run from the command
# line. When run it opens all the databases associated with
# the build system and print their contents to stdout.
if __name__ == "__main__":
    if len(sys.argv) == 2:
        print(binary_meta.get_report(sys.argv[1]))
    else:
        print("Usage: binary_meta_report.py binary.elf")
        sys.exit(1)
    sys.exit(0)
