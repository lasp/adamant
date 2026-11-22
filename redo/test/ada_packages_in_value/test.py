#!/usr/bin/env python3

# Unit test for packages_in_value in redo/util/ada.py: that it returns the
# packages an Ada value expression names, once each and in order, and none
# for values that name no package.
import sys

from util import ada

CASES = [
    ("(Value => 0.0)", []),
    ("[1.0, 0.5E-03, -2.0]", []),
    ("(Value => 16#DEAD#)", []),
    ("True", []),
    ("(Value => My_Enums.Mode.On)", ["My_Enums"]),
    ("[others => My_Enums.Mode.On]", ["My_Enums"]),
    ("[A_Enums.X.Y, B_Enums.X.Z, A_Enums.X.W]", ["A_Enums", "B_Enums"]),
    ("(Steps => [(Duration => 30.0, Axis => my_enums.Axis.X)])", ["My_Enums"]),
    ("My_Tables.Default_Table'Access", ["My_Tables"]),
    ("(Limit => Basic_Types.Byte'Last, Scale => 1.5)", ["Basic_Types"]),
]


def run_cases():
    failures = 0
    for value, expected in CASES:
        actual = ada.packages_in_value(value)
        if actual != expected:
            failures += 1
            print("FAILED: " + value + " gave " + str(actual) + ", expected " + str(expected), file=sys.stderr)
    if failures:
        sys.exit(1)
    print("passed.", file=sys.stderr)


if __name__ == "__main__":
    run_cases()
