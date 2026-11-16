#!/usr/bin/env bash
# Test that the valid memory maps generate and compile successfully, with their
# items at the expected addresses, and that the invalid memory maps fail.
set -e

# Keep redo target output empty; send all logs to stderr.
exec 1>&2

# Build the object for a memory map (not just source generation) so the
# Compile_Time_Error pragmas are checked.
expect_success() {
    echo "  Building $1 object (expect success)..."
    redo build/obj/Linux_Test/$1.o
    echo "  PASS: $1 object built successfully."
}

# Check that a generated memory map places an item at an address.
expect_address() {
    if grep -q "^   $2_Offset : constant Unsigned_32 := 16#$3#;" build/src/$1.ads; then
        echo "  PASS: $1 places $2 at 0x$3."
    else
        echo "  FAIL: $1 does not place $2 at 0x$3!"
        exit 1
    fi
}

# Check that a memory map fails to build, and that it fails for the expected reason.
expect_failure() {
    echo "  Building $1 object (expect failure)..."
    if output=$(redo build/obj/Linux_Test/$1.o 2>&1); then
        echo "  FAIL: $1 object built but should have failed!"
        exit 1
    elif echo "$output" | grep -q "$2"; then
        echo "  PASS: $1 correctly failed to build."
    else
        echo "  FAIL: $1 failed to build, but not with the expected error '$2':"
        echo "$output"
        exit 1
    fi
}

echo "Testing memory map generation..."

# A memory map with no offsets, alignment, or sizes lays its items out contiguously.
expect_success valid_memory_map
expect_address valid_memory_map Full_Width_Record 01000000
expect_address valid_memory_map Full_Width_Nested 01000007

# A memory map using offsets, alignment, reserved sizes, and a type that is not
# a packed type.
expect_success explicit_layout
expect_address explicit_layout First 03000000
expect_address explicit_layout Second 03000008
expect_address explicit_layout Early 03000040
expect_address explicit_layout Pinned 03000080
expect_address explicit_layout Raw 03000088
expect_address explicit_layout Unaligned 03000094
expect_address explicit_layout Final 030000F9

cd should_not_build
expect_failure invalid_memory_map "contains bit-constrained fields"
expect_failure overlapping_memory_map "Memory map overlap encountered"
expect_failure overrun_memory_map "Memory map exceeds the last memory address"
expect_failure misaligned_memory_map "is not aligned to 4 bytes"
expect_failure missing_size_memory_map "A 'size' must be provided"
expect_failure small_size_memory_map "is too small to hold type"
expect_failure small_raw_size_memory_map "does not fit in the 12 bytes reserved"
cd ..

echo "Memory map tests passed."
