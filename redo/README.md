# Redo

## Description

This directory contains a variety of python libraries which make up the [`redo`](https://github.com/dinkelk/redo)-based build system for the framework.

## Contents

The following is a description of what you can expect to find in the subdirectories of this directory.

* `base_classes/` - a set of python base classes from which many build system constructs inherit
* `bin/` - a set of python utilities which are useful when run by the user from the command line
* `database/` - python modules which make up the Key-Value (NoSQL) build system database
* `environments/` - python modules that are intended to be imported from `env.py` files which provide common environment settings
* `rules/` - a set of python classes, each which contains a rule to build a specific output file type
* `targets/` - a set of python classes, each which describes how to compile source code for different target platforms
* `test/` - a set of unit tests for ensuring the functionality of the build system
* `util/` - utility python modules used by other build system and generator python modules

## Configuring the Build System

The build system is configured based on the user's environment. The following sections will explain the types of configuration that are possible,
but all of these configurations are tunable based on the current environment. The environment can be configured in 3 different ways:

1. By the user from the command line setting the environment manually (or via a script) using `export VARIABLE=value` type commands. This method
has the advantage of being interactive and completely configurable by the user.
2. By writing custom redo rules in `.do` files which tailor the environment for certain rules.
3. By writing `env.py` files in a directory in which you wish to configure the environment manually. If a redo rule is run in a directory that
contains an `env.py` file, the `env.py` file is executed first, before the rule is executed. In this way, the user can set up the environment
for all rules that might be run in a specific directory separately from the rest of the system.

## The Build Target

The Adamant build system supports building binary object and executable files for multiple target platforms. For instance, two binaries built from the
same source code can be constructed in both a native Linux format and an embedded ARM based target. To configure which target is currently being built
you must set the `TARGET` variable to the name of the build target you wish to use. Available build target names can be found in the `targets/` directory or by running `redo targets`.
If `TARGET` is not set, then the build system will assume you want to build for the native target, and will use `uname` as the Target. This will usually
be the `Linux` target. The `TARGET` variable can be configured from the command line using `export TARGET=the_target` or from a local `.do` or `env.py` file.

## The Build Path

The Adamant build system uses a "build path" in the form `/path/to/directory1:/path/to/directory2:etc..` in order to find the appropriate source, object,
and model files for compiling and linking executables. The build path for a specific "redo" command can be configured in many different ways. The desired
method for build path calculation is communicated to the build system via environment variables which can be set from the command line, within a local `.do`
script, or within a local `env.py` script. The control variables are described below:

* `BUILD_PATH` - If this variable is set, the path described here in the form `/path/to/directory1:/path/to/directory2:etc..` is used as the entire build path. In this
way the user can customize the entire build path to fit their needs.
* `BUILD_ROOTS` - If `BUILD_PATH` is not set, but this variable IS set in the form `/path/to/root1:/path/to/root2:etc..`, then `BUILD_PATH` is calculated by recursively searching for "path files" from 
each build root directory. Every directory found that includes an applicable path file is automatically added to the build path. Applicable path files are simply empty files with the name 
`.all_path` or `.$TARGET_path`, where `$TARGET` is the name of the build target for which that path applies. In this way, source code designated with `.all_path`
can be compiled for any build target, where as source code designated with `.$TARGET_path` will only be compiled for that specific build target. This allows the user
to easily separate the platform specific code from the more portable platform agnostic code.

If neither of the above variables has been specified by the user then `BUILD_ROOTS` is automatically set by Adamant to be a path with two roots: 1) The root directory
of the Adamant repository, and 2) The root directory of the repository containing the current working directory (as determined by the location of the `.git` directory).
Calculation of the `BUILD_PATH` then follows in the same manner from these two directories as if `BUILD_ROOTS` were set manually by the user.

Note that the current working directory of any user issued redo command will always be added to the `BUILD_PATH` automatically.

It is often the case that for a specific build the user wants to add a path or add a build root to the `BUILD_PATH` that is calculated using the method. The following
environment variables allow the user to append extra directories to a build path.

* `EXTRA_BUILD_PATH` - If this variable is set, the path described here in the form `/path/to/directory1:/path/to/directory2:etc..` is added to `BUILD_PATH`
* `EXTRA_BUILD_ROOTS` - If this variable is set in the form `/path/to/root1:/path/to/root2:etc..` then all paths found using the recursive search method described
above for setting `BUILD_ROOTS` manually are appended to `BUILD_PATH`

In addition, it is also common to want to remove a specific directory from an already computed build path, using the previous environment variables. To do this 
a use can set the `REMOVE_BUILD_PATH` variable.

* `REMOVE_BUILD_PATH` - If this variable is set, the path described here in the form `/path/to/directory1:/path/to/directory2:etc..` is removed from `BUILD_PATH` after
it is calculated using the above methods.

This variable can be particularly useful if you want to override the functionality of a core package in Adamant, by first writing your own version of it and including
it in the build path, and then subsequently removing the path to the core package from the build path by adding it to `REMOVE_BUILD_PATH`. 

Note that Adamant will not allow two identically named packages to exist simultaneously in the build path. This feature, which can sometimes be annoying, is intended 
to prevent compiling ambiguous packages into a final binary. Due to this restriction, Users must be meticulous about only including unique package names in their project 
build paths.

## Build Debug Mode:

The build system has a debug mode in which verbose information about what is being executed by the build system is printed to the terminal during compilation. To
enable this mode simple set the `DEBUG` variable from the command line:

```
export DEBUG=1
```

To turn off the debug mode, you will need to unset the variable.

```
export DEBUG=
```
