## Test

#### Description

This directory contains a set of unit tests which ensure the functionality of the build system.

#### Contents

The following is a description of what you can expect to find in the subdirectories of this directory.

* `aspect_compile/` - a test which makes sure a child package declared with an aspect specification (which puts the `is` keyword on its own line) still has its implicit parent-package dependency discovered
* `c_compile/` - a test which makes sure the compilation of c source code and linking with an ada main file is working properly
* `source_dependencies/` - a unit test for the Ada source scanning in `redo/util/ada.py`, locking in the dependency extraction and body-dependency heuristics over a set of declaration shapes; runs under `redo test` like the python tests in `gen/test/`
