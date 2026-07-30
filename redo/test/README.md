## Test

#### Description

This directory contains a set of unit tests which ensure the functionality of the build system.

#### Contents

The following is a description of what you can expect to find in the subdirectories of this directory.

* `aspect_compile/` - a test which makes sure a child package declared with an aspect specification (which puts the `is` keyword on its own line) still has its implicit parent-package dependency discovered
* `c_compile/` - a test which makes sure the compilation of c source code and linking with an ada main file is working properly
