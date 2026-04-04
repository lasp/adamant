#!/usr/bin/env bash
# Test that the valid memory map generates and compiles successfully,
# and that the invalid memory map (bit-constrained types) fails.
set -e

# Keep redo target output empty; send all logs to stderr.
exec 1>&2

echo "Testing memory map Always_Valid enforcement..."

# Test 1: Valid memory map should build successfully
# Build object (not just source generation) so Compile_Time_Error pragmas are checked.
echo "  Building valid_memory_map object (expect success)..."
redo build/obj/Linux_Test/valid_memory_map.o
echo "  PASS: valid_memory_map object built successfully."

# Test 2: Invalid memory map should fail to build
# Build object target to force Ada compilation and trigger Compile_Time_Error.
echo "  Building invalid_memory_map object (expect failure)..."
cd should_not_build
if redo build/obj/Linux_Test/invalid_memory_map.o >/dev/null 2>&1; then
    echo "  FAIL: invalid_memory_map object built but should have failed!"
    exit 1
else
    echo "  PASS: invalid_memory_map correctly failed to build."
fi
cd ..

echo "Memory map Always_Valid tests passed."
