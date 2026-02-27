# Config

## Description

This directory contains configuration files used to tune specific parameters within Adamant to tailor it
for a specific project.

## Contents

The Adamant framework will not function without a configuration file used to defined certain core values.
A file `adamant.configuration.yaml.original` is included in this directory. This file shows the default
configuration values for the Adamant framework. To use the default values run:

```
$ cp adamant.configuration.yaml.original adamant.configuration.yaml
```

You can now modify the values in `adamant.configuration.yaml` to work for your project. Note that 
`adamant.configuration.yaml` has been added to .gitignore, and should NOT be checked into your
version control system if you intend on merging changes from your Adamant repository back into
the framework root repository.

Version controlling the configuration file is usually desirable. To accomplish this, copy the
Adamant default configuration into your project specific repository:

```
$ cp adamant.configuration.yaml.original /path/to/my/project/conf/my_project.configuration.yaml
```

You can now modify `my_project.configuration.yaml` and version control it within your project
directory. To tell Adamant to use this file instead of the default configuration file you need
to set the `ADAMANT_CONFIGURATION_YAML` variable, i.e.

```
$ export ADAMANT_CONFIGURATION_YAML=/path/to/my/project/conf/my_project.configuration.yaml
```

It is advisable to include the export command above within your environment files that get
sourced whenever a new shell is opened, i.e. from `~/.bashrc`.
