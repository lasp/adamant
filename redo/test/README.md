## Test

#### Description

This directory contains a set of unit tests which ensure the functionality of the build system.

#### Contents

The following is a description of what you can expect to find in the subdirectories of this directory.

* `aspect_compile/` - a test which makes sure a child package declared with an aspect specification (which puts the `is` keyword on its own line) still has its implicit parent-package dependency discovered
* `c_compile/` - a test which makes sure the compilation of c source code and linking with an ada main file is working properly
* `pregenerate/` - a unit test for the memory-aware scheduler in `redo/util/pregenerate.py`: that every target runs and reports its outcome, that the core-count cap and the memory admission rule bound how many generator workers run at once, that a starved host still progresses one worker at a time, that a worker the kernel kills is reported as failed instead of hanging the run, that a kind of target is probed one worker at a time until one has finished, that the per-kind expected size is learned from finished and running workers without one kind lowering another's and persists across batches in a process, that the kind of a work item is its input model type, and that the defaults are a worker per core and the placeholder expected size; runs under `redo test` like the python tests in `gen/test/`
* `pydep/` - a unit test for the python import scanning and dependency walk in `redo/util/pydep.py`: the names a file imports, the files an import loads, what is left out, that the build system and code generators are never reported or followed, and that a generated module is built from its model and put on the path; runs under `redo test` like the python tests in `gen/test/`
* `source_dependencies/` - a unit test for the Ada source scanning in `redo/util/ada.py`, locking in the dependency extraction and body-dependency heuristics over a set of declaration shapes; runs under `redo test` like the python tests in `gen/test/`
