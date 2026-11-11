## Test

#### Description

This directory contains a set of unit tests which ensure the functionality of the build system.

#### Contents

The following is a description of what you can expect to find in the subdirectories of this directory.

* `aspect_compile/` - a test which makes sure a child package declared with an aspect specification (which puts the `is` keyword on its own line) still has its implicit parent-package dependency discovered
* `assembly_submodel_cache/` - a unit test for how the model cache stores an assembly submodel in `gen/models/assembly.py`: that the submodel has no cache entry of its own, that it is built from its file until its assembly is cached, and that it then loads from the cached assembly with its assembly and dependencies in place; runs under `redo test` like the python tests in `gen/test/`
* `c_compile/` - a test which makes sure the compilation of c source code and linking with an ada main file is working properly
* `pydep/` - a unit test for the python import scanning and dependency walk in `redo/util/pydep.py`: the names a file imports, the files an import loads, what is left out, that the build system and code generators are never reported or followed, and that a generated module is built from its model and put on the path; runs under `redo test` like the python tests in `gen/test/`
* `shared_models/` - a unit test for model sharing in `gen/models/base.py`: that a record, enumeration, or array model is loaded once per process and the same object returned for every later load of its file, through `model_loader` or the model class itself, that ignoring the cache builds a fresh object, and that a component, which the assembly fills in with instance data, is built anew on every load; runs under `redo test` like the python tests in `gen/test/`
* `source_dependencies/` - a unit test for the Ada source scanning in `redo/util/ada.py`, locking in the dependency extraction and body-dependency heuristics over a set of declaration shapes; runs under `redo test` like the python tests in `gen/test/`
