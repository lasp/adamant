#!/usr/bin/env python3
import argparse
import sys

#
# This python utility is meant demonstrate non PEP8 (bad) style.
#
if __name__ == "__main__":
    # Parse the commandline arguments:
    parser=argparse.ArgumentParser(description="Test program that demonstrates bad style.")
    parser.add_argument(
        "file",
        metavar= "file.bin",
        type=str,
        help="A file.",
    )
    args = parser.parse_args()

    # Read the post mortem file:
    print("%s"%(args.file))
