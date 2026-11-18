## Test

#### Description

This directory contains a set of unit tests which ensure the functionality of the build system.

#### Contents

The following is a description of what you can expect to find in the subdirectories of this directory.

* `aspect_compile/` - a test which makes sure a child package declared with an aspect specification (which puts the `is` keyword on its own line) still has its implicit parent-package dependency discovered
* `assembly_submodel_cache/` - a unit test for how the model cache stores an assembly submodel in `gen/models/assembly.py`: that the submodel has no cache entry of its own, that it is built from its file until its assembly is cached, and that it then loads from the cached assembly with its assembly and dependencies in place; runs under `redo test` like the python tests in `gen/test/`
* `c_compile/` - a test which makes sure the compilation of c source code and linking with an ada main file is working properly
* `pregenerate/` - a unit test for the memory-aware scheduler in `redo/util/pregenerate.py`. It checks that core count and free memory bound how many generator workers run at once, and that a worker the kernel kills fails its target without hanging the build. It also covers how each model type's growth beyond the process that forks it is probed, learned, and saved between build steps; runs under `redo test` like the python tests in `gen/test/`
* `model_validation/` - a unit test for schema validation in `gen/models/base.py`: that a well formed model loads, that a model with an unknown key, a missing required key, or a value of the wrong type is rejected with an error naming its file, that validation leaves the model's parsed contents as they were read, and that a schema is parsed once per process and reused; runs under `redo test` like the python tests in `gen/test/`
* `pydep/` - a unit test for the python import scanning and dependency walk in `redo/util/pydep.py`: the names a file imports, the files an import loads, what is left out, that the build system and code generators are never reported or followed, and that a generated module is built from its model and put on the path; runs under `redo test` like the python tests in `gen/test/`
* `shallow_assembly_cache/` - a unit test for how the model cache stores a shallow load of an assembly in `gen/models/assembly.py`: that it is built from its file once per session and then loaded from its own cache entry, that each load gets its own object, that it is never used for a full load, that a load asked to ignore the cache or to shallow load only some components is built from its file, and that a cached full load is preferred; runs under `redo test` like the python tests in `gen/test/`
* `shared_models/` - a unit test for model sharing in `gen/models/base.py`: that a record, enumeration, or array model is loaded once per process and the same object returned for every later load of its file, through `model_loader` or the model class itself, that ignoring the cache builds a fresh object, and that a component, which the assembly fills in with instance data, is built anew on every load; runs under `redo test` like the python tests in `gen/test/`
* `source_dependencies/` - a unit test for the Ada source scanning in `redo/util/ada.py`, locking in the dependency extraction and body-dependency heuristics over a set of declaration shapes; runs under `redo test` like the python tests in `gen/test/`
* `view_filters/` - a unit test for view filters in `gen/models/view.py`: that applying each filter, the not, group, and and or combinations, or a whole view leaves the assembly it was applied to unchanged and gives the same result as on a freshly loaded assembly, which the shallow copy each filter works on relies on, and that every entry of `filter_type_dict` has a case; runs under `redo test` like the python tests in `gen/test/`
