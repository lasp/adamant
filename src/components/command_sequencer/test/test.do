#!/usr/bin/env python

# Build the test sequences:
from util import redo
redo.redo_ifchange("test_sequences/all")

# Run the normal redo test
import sys
from rules.build_test import build_test
rule = build_test()
rule.build(*sys.argv[1:])
