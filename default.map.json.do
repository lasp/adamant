#!/usr/bin/env python3

try:
    from util import performance
except ModuleNotFoundError:
    import sys
    sys.stderr.write("Adamant environment not set up! Run:\n    source ~/adamant/env/activate\n")
    sys.exit(1)
# Optimize python path:
performance.optimize_path()

# Imports
import sys
from rules.build_map import build_map

# This .do file builds .map.json address maps of executables.

if __name__ == "__main__":
    assert len(sys.argv) == 4
    rule = build_map()
    rule.build(*sys.argv[1:])

# Exit fast:
performance.exit(sys.argv[2])
