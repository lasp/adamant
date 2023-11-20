#!/usr/bin/env python3
import argparse
from util import crc_16

#
# This python utility is meant to be run from the command
# line. It produces a standard CRC-16 for an input binary file.
#
# Uses CCITT CRC polynomial: X^16 + X^12 + X^5 + 1.
# The accepted start value is 0xffff.
#
if __name__ == "__main__":
    # Parse the commandline arguments:
    parser = argparse.ArgumentParser(description="Calculate the CRC-16 for a file.")
    parser.add_argument(
        "file",
        metavar="file.bin",
        type=str,
        help="The file (treated as binary) to calculate a CRC-16 on.",
    )
    args = parser.parse_args()

    # Read the post mortem file:
    content = None
    with open(args.file, mode="rb") as f:
        content = f.read()
    crc = crc_16.crc_16(content)
    assert len(crc) == 2
    crcint = crc[0] * 256 + crc[1]
    print("0x%04x" % (crcint))
