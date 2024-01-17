#!/usr/bin/env python

# Optimize python path:
from util import performance
performance.optimize_path()

# Imports
import sys
from rules.build_gpr import build_gpr

# This .do file builds .gpr files.

if __name__ == "__main__":
    assert len(sys.argv) == 4
    rule = build_gpr()
    rule.build(*sys.argv[1:])

# Exit fast:
performance.exit(sys.argv[2])
