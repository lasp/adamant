#!/usr/bin/env python
import sys
import glob
import os
from util import redo_arg
from util import target
from util import redo
from rules.build_object import build_object

if __name__ == "__main__":
  assert len(sys.argv) == 4
  # Before we build the objects in this directory,
  # we need to build the dependencies we have on
  # the C source in the subdirectory.
  deps = []
  for file in glob.iglob(".." + os.sep + "c_src" + os.sep + "*.c"):
    ofile = redo_arg.src_file_to_obj_file(file, target.get_default_target())
    deps.append(file)
    deps.append(ofile)
  for file in glob.iglob(".." + os.sep + "c_src" + os.sep + "*.c.do"):
    file = file[:-3]
    ofile = redo_arg.src_file_to_obj_file(file, target.get_default_target())
    deps.append(file)
    deps.append(ofile)
  redo.redo_ifchange(deps)

  # Now build this object:
  rule = build_object()
  rule.build(*sys.argv[1:])
