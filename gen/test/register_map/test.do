#!/usr/bin/env bash
# Test that the valid register map generates and compiles successfully,
# and that the invalid register map (bit-constrained types) fails.
set -e

# Keep redo target output empty; send all logs to stderr.
exec 1>&2

echo "Testing register map Always_Valid enforcement..."

# Test 1: Valid register map should build successfully
# Build object (not just source generation) so Compile_Time_Error pragmas are checked.
echo "  Building valid_register_map object (expect success)..."
redo build/obj/Linux_Test/valid_register_map.o
echo "  PASS: valid_register_map object built successfully."

# Test 2: Invalid register map should fail to build
# Build object target to force Ada compilation and trigger Compile_Time_Error.
echo "  Building invalid_register_map object (expect failure)..."
cd should_not_build
if redo build/obj/Linux_Test/invalid_register_map.o >/dev/null 2>&1; then
    echo "  FAIL: invalid_register_map object built but should have failed!"
    exit 1
else
    echo "  PASS: invalid_register_map correctly failed to build."
fi
cd ..

echo "Register map Always_Valid tests passed."
