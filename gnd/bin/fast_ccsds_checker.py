#!/usr/bin/env python3
import argparse
import time
from util import crc_16
import mmap
import os.path
import signal
import sys
from collections import OrderedDict

#
# A very fast (for python) swiss army knife like ccsds checker.
# Run the help for all the configuration options.
#


class Sys_Time(object):
    def __init__(self, seconds, subseconds):
        self.seconds = seconds
        self.subseconds = subseconds


def delta_time(first_time, second_time):
    if first_time is None or second_time is None:
        return None
    duration_seconds = second_time.seconds - first_time.seconds
    duration_subseconds = second_time.subseconds - first_time.subseconds
    if duration_subseconds < 0:
        duration_seconds -= 1
        duration_subseconds = 2**32 - duration_subseconds
    return Sys_Time(seconds=duration_seconds, subseconds=duration_subseconds)


def ts_string(a_time):
    if a_time is not None:
        return "%10d.%06d" % (a_time.seconds, a_time.subseconds / 2**32 * 1000000)
    return "None"


class packet_data(object):
    def __init__(self, packet_apid):
        self.count = 0
        self.expected_sequence_count = 0
        self.sequence_count_errors = 0
        self.apid = packet_apid
        self.first_time = None
        self.time = None
        self.packet_length_min = None
        self.packet_length_max = None
        self.missing_sequence_counts = []
        self.min_sequence_count = None
        self.max_sequence_count = None
        self.first_sequence_count = None
        self.last_sequence_count = None

    # def add(self, packet, check_sequence_counts=True):
    def add(
        self,
        packet_apid,
        packet_length,
        packet_sequence_count,
        packet_secondary_header,
        packet_time,
        check_sequence_counts=True,
    ):
        assert packet_apid == self.apid, (
            "Expected packet with apid "
            + str(self.apid)
            + " but got "
            + str(packet_apid)
        )
        # Increment packet count:
        self.count += 1
        # Check sequence count:
        if check_sequence_counts:
            if self.first_time is not None:
                if self.expected_sequence_count != packet_sequence_count:
                    self.sequence_count_errors += 1
                    self.missing_sequence_counts.append(self.expected_sequence_count)
            self.expected_sequence_count = (packet_sequence_count + 1) % 2**14
        if self.first_sequence_count is None:
            self.first_sequence_count = packet_sequence_count
        self.last_sequence_count = packet_sequence_count
        if self.min_sequence_count is None:
            self.min_sequence_count = packet_sequence_count
        else:
            if packet_sequence_count < self.min_sequence_count:
                self.min_sequence_count = packet_sequence_count
        if self.max_sequence_count is None:
            self.max_sequence_count = packet_sequence_count
        else:
            if packet_sequence_count > self.max_sequence_count:
                self.max_sequence_count = packet_sequence_count
        # Save time stamps
        if packet_secondary_header:
            self.time = packet_time
            if self.first_time is None:
                self.first_time = self.time
        # Save packet lengths:
        if self.packet_length_min is None:
            self.packet_length_min = packet_length
        else:
            if packet_length < self.packet_length_min:
                self.packet_length_min = packet_length
        if self.packet_length_max is None:
            self.packet_length_max = packet_length
        else:
            if packet_length > self.packet_length_max:
                self.packet_length_max = packet_length


# Global state:
packets = []
packet_count = 0
packet_error_count = 0
packet_header_invalid_count = 0
packet_crc_invalid_count = 0
apid_dict = {}  # Map of apid to packet data class
first_time = Sys_Time(0, 0)
current_time = Sys_Time(0, 0)


def parseCcsds(
    content,
    offset,
    apid_list=[],
    sync_bytes=14,
    max_packet_length=3000,
    min_packet_length=0,
    max_packets_to_parse=None,
    max_bytes_to_parse=None,
    use_checksum=False,
):
    # Reference globals:
    global packet_count
    global packet_count
    global packet_error_count
    global packet_header_invalid_count
    global packet_crc_invalid_count
    global first_time
    global current_time

    # The binary data should be a set of CCSDS packets, so extract those:
    idx = 0
    errored = False
    error_end_idx = 0
    # error_start_idx = 0
    while idx < len(content):
        # Check limits:
        if max_packets_to_parse is not None and packet_count >= max_packets_to_parse:
            break
        if max_bytes_to_parse is not None and idx >= max_bytes_to_parse:
            break

        # Check for a valid header:
        header_valid = False
        apid = None
        length = None
        sequence_count = None
        secondary_header = None
        if idx + 5 < len(content):
            # Pull out the APID and length using mask and shift
            apid = ((content[idx + 0] << 8) + content[idx + 1]) & 0x7FF
            length = (content[idx + 4] << 8) + content[idx + 5]
            # Extract sequence count and secondary header flag
            sequence_count = ((content[idx + 2] << 8) + content[idx + 3]) & 0x3FFF
            secondary_header = content[idx] & 0x8
            if (
                (not apid_list or apid in apid_list)
                and (not max_packet_length or length <= max_packet_length)
                and (not min_packet_length or length >= min_packet_length)
            ):
                header_valid = True

        # If header is valid, extract crc.
        crc_valid = False
        crc = None
        packet_bytes_array = None
        packet_data_bytes = None
        if header_valid:
            # Create an array of the data bytes and of the whole packet for convenience:
            packet_bytes_array = content[idx:(idx + length + 7)]
            packet_data_bytes = content[(idx + 6):(idx + length + 7)]

            # Calculate CRC using appropriate algorithm.
            try:
                if use_checksum:
                    crc = crc_16.checksum_16(packet_bytes_array[0:-2])
                else:
                    crc = crc_16.crc_16(packet_bytes_array[0:-2])
            except AssertionError as e:
                print(str(e))

            # Validate crc of packet:
            if crc and packet_data_bytes[-2:] == bytes(crc):
                crc_valid = True

        # If the crc is valid, then continue processing.
        if crc_valid:
            # Handle any packet errors if any occurred:
            if errored:
                # print(str(error_end_idx - error_start_idx) + " bytes of invalid data found between index " + str(
                #  error_start_idx + offset) + " and " + str(error_end_idx - 1 + offset) + ".")
                errored = False

            # Extract time:
            current_time = None
            if secondary_header:
                current_time = Sys_Time(
                    packet_data_bytes[0] * 16777216
                    + packet_data_bytes[1] * 65536
                    + packet_data_bytes[2] * 256
                    + packet_data_bytes[3],
                    packet_data_bytes[4] * 16777216
                    + packet_data_bytes[5] * 65536
                    + packet_data_bytes[6] * 256
                    + packet_data_bytes[7],
                )
                if packet_count == 0:
                    first_time = current_time

            # Increment the index by the number of sync bytes between packets, since we found a valid packet
            idx += len(packet_bytes_array) + sync_bytes
            packet_count += 1

            # Add to APID dict:
            try:
                apid_dict[apid].add(
                    apid, length, sequence_count, secondary_header, current_time
                )
            except KeyError:
                apid_dict[apid] = packet_data(apid)
                apid_dict[apid].add(
                    apid, length, sequence_count, secondary_header, current_time
                )

            # Print packet:
            if sequence_count > (11327 - 10) and sequence_count < (11327 + 10):
                print("apid: " + str(apid))
                print("length: " + str(length))
                print("array_length: " + str(len(packet_bytes_array)))
                print("seq cnt: " + str(sequence_count))
                print("timestamp:  " + ts_string(current_time))
                print("crc: " + str(list(crc)))
                print("bytes: ")
                print(str([hex(x) for x in packet_bytes_array]))

        # Packet was not successfully extracted because either header or CRC was no good.
        else:
            if errored:
                # Search for the next valid packet one byte at a time:
                idx += 1
                error_end_idx += 1
            else:
                errored = True
                # error_start_idx = idx
                error_end_idx = idx
                packet_error_count += 1
                # print("---------------------------------------------------------------------------------")
                # print("Parse error found at global index: " + str(idx + offset) + ", chunk index: " + str(idx))
                # print("---------------------------------------------------------------------------------")
                # print("Number of Packets parsed: " + str(packet_count))
                # print("-- Configuration --")
                # print("APID List: " + str(apid_list))
                # print("Packet min length limit: " + str(min_packet_length))
                # print("Packet max length limit: " + str(max_packet_length))
                # print("-- Error Information --")
                # print("Header bytes: " + str(list(content[idx:idx+6])) + \
                #       " (" + str([hex(x) for x in content[idx:idx+6]]) + ")")
                # print("Packet Apid: " + str(apid) + " Packet Length: " + \
                #       str(length) + " Sequence Count: " + str(sequence_count))
                # print("Header validated?: " + str(header_valid))
                # print("CRC validated?: " + str(crc_valid))
                # if crc:
                #  print("Packet CRC: " + str(list(packet_data_bytes[-2:]))  + " Computed CRC: " + str(crc))
                # print("---------------------------------------------------------------------------------")
                if not header_valid:
                    packet_header_invalid_count = packet_header_invalid_count + 1
                elif not crc_valid:
                    packet_crc_invalid_count = packet_crc_invalid_count + 1

    return packet_count, idx


# This python utility is meant to be run from the command
# line. When run it decodes a raw record file of CCSDS packets,
# reports any errors that it finds and prints some statistics
# to the commandline:
if __name__ == "__main__":
    class ExtendAction(argparse.Action):
        """
        Arparse extend definition:
        https://stackoverflow.com/questions/41152799/argparse-flatten-the-result-of-action-append
        """
        def __call__(self, parser, namespace, values, option_string=None):
            items = getattr(namespace, self.dest) or []
            items.extend(values)
            setattr(namespace, self.dest, items)

    # Parse the commandline arguments:
    parser = argparse.ArgumentParser(
        description="Decode a raw dump of CCSDS, report any errors, and show the APIDs found."
    )
    parser.register("action", "extend", ExtendAction)
    parser.add_argument(
        "raw_record_file",
        metavar="raw_record.bin",
        type=str,
        help="The raw record file of CCSDS packets.",
    )
    parser.add_argument(
        "-n",
        action="store",
        type=int,
        help=("Specify the maximum number of packets to parse before exiting. If not "
              "specified, the whole file is parsed."),
    )
    parser.add_argument(
        "-b",
        action="store",
        type=int,
        help="Specify the maximum number of bytes to parse before exiting. If not specified, the whole file is parsed.",
    )
    parser.add_argument(
        "-c",
        action="store",
        type=int,
        default=1000000,
        help=("Specify the chunk size to use when parsing. This is the amount of read in from the file and parsed in "
              "memory at any given time. Choosing the correct value can greatly increase parsing speed. The default "
              "value is 1000000 bytes."),
    )
    parser.add_argument(
        "-q",
        action="store_true",
        default=False,
        help="Quiet. Disables progress messages while parsing.",
    )
    parser.add_argument(
        "--checksum16",
        action="store_true",
        help=("Parse CCSDS packets that have a 16-bit checksum. If not specified, the default behavior is to look for "
              "a 16-bit CRC. There is no option to disable CRC checking at this time."),
    )
    parser.add_argument(
        "--apids",
        action="extend",
        nargs="+",
        type=int,
        default=[],
        help="CCSDS APIDs to consider valid. Others will be considered invalid. By default any APID is acceptable.",
    )
    parser.add_argument(
        "--num-pre-bytes",
        action="store",
        type=int,
        default=0,
        help="The number of extra bytes to ignore before the beginning of each packet. Default: 0",
    )
    parser.add_argument(
        "--num-post-bytes",
        action="store",
        type=int,
        default=0,
        help="The number of extra bytes to ignore at the end of each packet. Default: 0",
    )
    parser.add_argument(
        "--min-packet-length",
        action="store",
        type=int,
        default=None,
        help=("The minimum size of an acceptable CCSDS packet (compared against CCSDS Packet Length field). "
              "Default: None"),
    )
    parser.add_argument(
        "--max-packet-length",
        action="store",
        type=int,
        default=None,
        help=("The maximum size of an acceptable CCSDS packet (compared against CCSDS Packet Length field). "
              "Default: None"),
    )
    parser.add_argument("--foo", help="foo help")
    args = parser.parse_args()
    assert args.c >= 5000, "Error: Please specify a chuck size of at least 5000 bytes."

    # Open the file
    with open(args.raw_record_file, mode="rb") as f:
        # Get the file size. If the file is empty tell the user.
        file_size = os.path.getsize(args.raw_record_file)
        if file_size > 0:
            mm = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
        else:
            print("File is empty.")
        total_bytes_to_parse = file_size
        if args.b and args.b < file_size:
            total_bytes_to_parse = args.b
        idx = args.num_pre_bytes

        def print_summary():
            """Print summary of results for user."""
            print("")
            print(
                "---------------------------------------------------------------------------------"
            )
            print("Summary")
            print(
                "---------------------------------------------------------------------------------"
            )
            print("Number of valid packets parsed: " + str(packet_count))
            print("Number of bytes parsed:         " + str(idx))
            print("Number APIDS found:             " + str(len(apid_dict)))
            print("Number of packet parse errors:  " + str(packet_error_count))
            print("Number of invalid headers:      " + str(packet_header_invalid_count))
            print("Number of invalid CRCs:         " + str(packet_crc_invalid_count))
            print("")
            print("Timestamp Info")
            print("First timestamp: " + ts_string(first_time))
            print("Last timestamp:  " + ts_string(current_time))
            print("Duration:        " + ts_string(delta_time(first_time, current_time)))
            print("")
            print("APID Table")
            print(
                ("        APID       Count  SC Errors  SC Min  SC Max  SC First  SC Last  PL Min  "
                 "PL Max          First TS           Last TS          Delta TS")
            )
            apids = sorted(apid_dict.keys())
            apid_data = [apid_dict[a] for a in apids]
            for data in apid_data:
                print(
                    "%4d (0x%03x)  %10d       %4d   %5d   %5d     %5d    %5d    %4d    %4d %s %s %s"
                    % (
                        data.apid,
                        data.apid,
                        data.count,
                        data.sequence_count_errors,
                        data.min_sequence_count,
                        data.max_sequence_count,
                        data.first_sequence_count,
                        data.last_sequence_count,
                        data.packet_length_min,
                        data.packet_length_max,
                        ts_string(data.first_time),
                        ts_string(data.time),
                        ts_string(delta_time(data.first_time, data.time)),
                    )
                )
            print("")
            print("Missing Sequence Count Table")
            for data in apid_data:
                print("        APID       Missing Sequence Counts")
                print("  %4d(0x%03x)      " % (data.apid, data.apid), end="")
                if data.missing_sequence_counts:
                    for seq in list(OrderedDict.fromkeys(data.missing_sequence_counts)):
                        print("%4d(0x%x), " % (seq, seq), end="")
                    print("")
                else:
                    print("None")
            print("")
            print(
                "---------------------------------------------------------------------------------"
            )
            print("")

        def print_summary_exit(*args):
            """Register control C to just print the summary so far and exit."""
            print("")
            print("Exiting after printing current parse summary...")
            print_summary()
            sys.exit(0)

        signal.signal(signal.SIGINT, print_summary_exit)

        # Read the file using chunking with mmap for speed:
        content = None
        execution_time = time.time()
        while idx < total_bytes_to_parse:
            # Figure out how many bytes we should read out to process the next chunk of data:
            bytes_left_to_read = total_bytes_to_parse - idx
            bytes_to_read = args.c
            max_bytes_to_parse = bytes_to_read - 2000
            assert max_bytes_to_parse > 0
            if bytes_to_read > bytes_left_to_read:
                bytes_to_read = bytes_left_to_read
                max_bytes_to_parse = None

            # Read out some bytes:
            mm.seek(idx)
            content = mm.read(bytes_to_read)

            # Parse packets from the content:
            # print("processing chunk: " + str(idx) + " -> " + str(idx + bytes_to_read))
            num_packets, num_bytes_parsed = parseCcsds(
                content,
                offset=idx,
                apid_list=args.apids,
                sync_bytes=args.num_pre_bytes + args.num_post_bytes,
                max_packet_length=args.max_packet_length,
                min_packet_length=args.min_packet_length,
                max_packets_to_parse=(args.n if args.n else None),
                max_bytes_to_parse=max_bytes_to_parse,
                use_checksum=args.checksum16,
            )

            # Increment the position in the file.
            if num_bytes_parsed == 0:
                break
            idx += num_bytes_parsed  # Account for sync bytes
            if bytes_to_read == bytes_left_to_read:
                break

            # Print a progress to the user.
            if not args.q and (time.time() - execution_time) > 2.0:
                max_bytes = (
                    max_bytes_to_parse if max_bytes_to_parse is not None else len(content)
                )
                print(
                    "Parsing at index="
                    + str(idx)
                    + "/"
                    + str(total_bytes_to_parse)
                    + (", percent complete=%02.1f" % (idx / total_bytes_to_parse * 100))
                    + ", packets parsed="
                    + str(num_packets)
                )
                execution_time = time.time()

            # Flush output, important for showing progress prints when calling this a script.
            sys.stdout.flush()

    print_summary()
