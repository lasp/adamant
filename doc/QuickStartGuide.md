# Quick Start Guide
The purpose of this README.md is to provide a quick reference for doing things within the Adamant system. Specifically, it will explain the necessary `redo` build commands used to perform certain tasks. Many of the tasks described here are also described in the User Guide, but in much more detail. This guide omits detailed explanations, and instead encourages self exploration by the developer, in order to keep this document small and easy to search.

## Useful Adamant Directories

* **Model Schemas** (`generators/schemas`) - describes the available fields in each model file type
* **Components** (`src/components`) - a set of reusable component provided with Adamant
* **Packed Types** (`src/types`) - a set of Ada types and packed types using in the Adamant framework
* **Build Targets** (`redo/targets`) - the build target files provided with the Adamant framework

## Getting Started

When solving an embedded problem your goal is usually to make an executable to run a system in production or through a specific test. Adamant executables consist of a hand-written main which runs an assembly of components. Each of these components communicate with one another through connectors which specify specific types passed between them. With this top down view in mind, an executable is usually constructed from the ground up in the following order.

1. Design, on a white board, the different components in your system, and how they will interact in an assembly from a high level.
2. Begin creating a single component by first creating all of the packed types and/or regular types that will be passed along its connectors. See the "Creating a Packed Type" section for details.
3. Create the actual component model by following the procedure in "Creating a Component."
4. Create component documentation and unit test a component by following the procedures in "Creating Component Documentation" and "Unit Testing a Component."
5. Repeat steps 2-4 for each component in your system.
6. Create an assembly by following the procedures in "Creating an Assembly."
7. Run/test your assembly by following the procedures in "Creating a Main Executable."

The sections included in this guide should outline in more detail how you might accomplish any of the steps listed above.

## Build System Basics

Adamant utilizes the [`redo`](https://github.com/dinkelk/redo) build system in order to generate/compile source code and create documentation. Anything in the "build path" can be acted upon by redo. Note, that in Adamant, the current directory is always included in the build path. For more details about the "build path" see the following section.

In Adamant, anything constructed by the build system will always be put into a directory called `build/`, usually directly below the entity that is used to create that output. For example, when the build system finds a source file called `source_file.ads` in a directory `dir`, a rule will be constructed to compile `source_file.ads` into the object `dir/build/obj/Linux/source_file.o`. The `build/` directory usually will contain subdirectories which group certain file outputs, i.e. `build/obj/` for objects, `build/src/` for source files, `build/html/` for html files, etc. This allows the user to quickly locate and inspect generated files. "Cleaning" an Adamant system is achieved by simply removing all of the `build/` directories in the system. `build/` directories are added to Adamant's `.gitignore` file so that they can never be checked in. In general, checking in generated files is considered bad practice. The alternative, consistently generating outputs from their source, ensures that the system is always up to date.

There are some basic `redo` commands available in Adamant that are almost always available, and can be particularly useful:

* `redo what` - The build system will explore the build path and print a list of the available outputs that can be built from the current directory. The list returned is not guaranteed to be complete, but this deficiency should not hinder anyone except for the most advanced Adamant users.
* `redo all` - The build system will try to generate every output available within a directory's `build/` directory. What `redo all` will construct can be visualized by using `redo what`. Note, running simply `redo` is assumed by `redo` to be equivalent to running `redo all`.
* `redo clean` - This removes any `build/` directories found recursively from where it is run.
* `redo test_all` - Recursively find any unit tests from where this command is run, compile them, and run them, outputting a simple pass/fail report to the terminal.
* `redo publish` - Recursively "publish" any documentation found from this directory. For more detail on "publishing" documentation see the "Creating Component Documentation" section.
* `redo style` - Run style checks on all source files (Python, Ada, and YAML) found in this directory.
* `redo targets` - Show all available build targets, see the "Setting the Build Target" section.

## Adding to the Build Path

Adamant can construct generated output or compiled objects/binaries for any models or source code in the build path. For details about configuring and controlling the build path see this [README.md](../redo/README.md) in `redo/`. Adding a directory to the build path usually amounts to adding a path file into that directory. To add a directory to the build path for all build targets you can run:

```
$ touch .all_path
```

in that directory. To add a file to the build path for a specific target, such as the "Linux" build path, you can run:

```
$ touch .Linux_path
```

For more information on build targets see the "Setting the Build Target" section.

## Setting the Build Target

The build target in Adamant tells the build system how to compile Ada/C/C++, including which compiler to use, which flags to use, etc. Available build targets can be found by running `redo targets`. The `TARGET` environment variable is used to control the current build target. For example setting:

```
$ export TARGET=Pico
```

will tell the build system to compile for the Raspberry Pi Pico by default. However, unsetting `TARGET`:

```
$ export TARGET=
```

will cause the build system to use the default target, which is computed using the current computer's `uname`, which will usually be `Linux`. So by default, Adamant will compile everything for the native machine.

Note, that setting `TARGET` above only sets the *default* target, i.e. the one which is chosen when you run vague commands like `redo all` or `redo what`. You can always compile an object for any target that you like by running a more specific command like:

```
$ redo build/obj/<insert_some_specific_target>/source_file_name.o
```

which would compile the object using the `<insert_some_specific_target>` target, if it exists.

## Creating a Packed Type

A packed record or array can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. To create a packed record, make a file of the name:

```
$ touch <insert_record_name>.record.yaml
```

To create a packed array, make a file of the name:

```
$ touch <insert_array_name>.array.yaml
```

Usually, it will be easiest to start with an existing packed model, and from there create your own. There are many examples in `src/types`. Next you can use your packed type model to generate many different output products. Simply run `redo what` in the same directory as the model file and you should get an output like:

```
$ redo what
redo all
redo clean
redo build/html/<packed_type_name>.html
redo build/obj/Linux/<packed_type_name>-representation.o
redo build/obj/Linux/<packed_type_name>.o
redo build/src/<packed_type_name>-representation.adb
redo build/src/<packed_type_name>-representation.ads
redo build/src/<packed_type_name>.ads
redo build/tex/<packed_type_name>.tex
```

From here you can build an html and latex representation of the packed type, the packed type Ada spec (.ads) file, the Ada type "representation" packages sources which allow you to print a packed type in human readable form, and the objects that correspond to the compiled source. Simply copy any of the `redo` commands from `redo what` to produce your desired output.

## Creating a Component

A component can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. Usually, you will want to create a component in its own directory. To create a component, make a file of the name:

```
$ touch <insert_component_name>.component.yaml
```

Usually, it will be easiest to start with an existing component, and from there create your own. There are many examples in `src/components`. Before creating a component model, it is advisable to create any packed types that you might use as "connector types" for that component. See "Creating a Packed Type" for more details.

Next you can use your component model to generate many different output products. Simply run `redo what` in the same directory as the model file and you should get an output like:

```
$ redo what
redo all
redo clean
redo build/dot/<component_name>.dot
redo build/eps/<component_name>.eps
redo build/obj/Linux/component-<component_name>-implementation.o
redo build/obj/Linux/component-<component_name>.o
redo build/png/<component_name>.png
redo build/src/component-<component_name>.adb
redo build/src/component-<component_name>.ads
redo build/svg/<component_name>.svg
redo build/template/component-<component_name>-implementation.adb
redo build/template/component-<component_name>-implementation.ads
```

From here you can generate the component diagram by running `redo build/svg/<component_name>.svg`. You can then open this output in a web browser to view the diagram. ".png" and ".eps" versions of the diagram can also be built using the appropriate `redo` commands, as seen above.

The base class source code can be constructed by running `redo build/src/component-<component_name>.adb` and `redo build/src/component-<component_name>.ads`. This package contains the structural code for the component. You can compile this code into an object by running `redo build/obj/Linux/component-<component_name>.o`.

You are responsible, as a developer, for writing the implementation package source code. However, a template for this code can be generated by running `redo templates`. You should then copy these files into the same directory as your model file, i.e.

```
$ cp build/template/* .
```

Now you can begin writing you implementation code inside of the copied templates. You can compile your implementation package by running `redo build/obj/Linux/component-<component_name>-implementation.o`.

Note, you can build everything in the component directory by running `redo all`, however, the implementation package will not compile unless you have built the templates in `build/template` and copied them into the same directory as the component model file.

## Creating Component Documentation

Component documentation must be created in a `doc/` directory below the location of a component's model file. To create documentation you must first make this directory and `cd` into it:

```
$ mkdir -p doc
$ cd doc
```

From here you can run `redo what` to see what you can build.

```
$ redo what
redo all
redo clean
redo publish
redo build/pdf/<component_name>.pdf
redo build/template/<component_name>.tex
redo build/tex/<component_name>_commands.tex
redo build/tex/<component_name>_connectors.tex
redo build/tex/<component_name>_data_products.tex
redo build/tex/<component_name>_description.tex
redo build/tex/<component_name>_events.tex
redo build/tex/<component_name>_init.tex
redo build/tex/<component_name>_preamble.tex
redo build/tex/<component_name>_stats.tex
redo build/tex/<component_name>_types.tex
redo build/tex/<component_name>_unit_test.tex
```

From here you can build many latex (.tex) output files which are generated from the component's model. To build the component documentation (.pdf) you are first responsible for hand writing the latex master document file. A template for this file can be generated by running `redo templates`. This file should be copied into the `doc` directory by running:

```
$ cp build/template/* .
```

From here you can modify the `<component_name>.tex` file to your heart's content. However, the generated template is quite good, and might suffice for many components. You can now build the component documentation by running `redo build/pdf/<component_name>.pdf`.

Note that anything in a `build/` directory is removed upon `redo clean` and will not be saved. "Published" documentation must be copied out of `build/pdf` and added to the version control system in a more permanent directory like `doc/`. To automatically build the documentation and copy it to this location, simply run `redo publish`.

## Unit Testing a Component

First follow the procedure to create a component in "Creating a Component". Unit tests for a component can be created anywhere, but the convention is to create them in a directory called `test/` below the component model file. To do this we might run:

```
$ mkdir -p test
$ cd test
```

From this directory, we need to create a unit test model file of the form:

```
$ touch <insert_component_name>.tests.yaml
```

Alternatively, if you want a specific name for your test suite, maybe because you are planning using more than one suite you can do:

```
$ touch <insert_unit_test_suite_name>.<insert_component_name>.tests.yaml
```

Unit test models are simple. Copy one from an existing component and modify it to meet the testing that you need to do. Note: You will want to make sure the common `env.py` file that is included in most unit test directories is included in your unit test directory. Otherwise you will need to run `export TARGET=Linux_Test` before running any of the commands below to make sure you are using the unit test build target, which includes the needed testing libraries (i.e. AUnit). Now we can run `redo what` to determine what we can build:

```
$ redo what
redo all
redo clean
redo build/bin/Linux_Test/test.elf
redo build/html/<component_name>_tests.html
redo build/obj/Linux_Test/component-<component_name>-implementation-tester.o
redo build/obj/Linux_Test/component-<component_name>_reciprocal.o
redo build/obj/Linux_Test/test.o
redo build/obj/Linux_Test/<component_name>_tests-implementation-suite.o
redo build/obj/Linux_Test/<component_name>_tests-implementation.o
redo build/obj/Linux_Test/<component_name>_tests.o
redo build/src/component-<component_name>_reciprocal.adb
redo build/src/component-<component_name>_reciprocal.ads
redo build/src/<component_name>_tests-implementation-suite.adb
redo build/src/<component_name>_tests-implementation-suite.ads
redo build/src/<component_name>_tests.adb
redo build/src/<component_name>_tests.ads
redo build/template/component-<component_name>-implementation-tester.adb
redo build/template/component-<component_name>-implementation-tester.ads
redo build/template/test.adb
redo build/template/<component_name>_tests-implementation.adb
redo build/template/<component_name>_tests-implementation.ads
redo test
```

From here you can generate unit test documentation in the form of html by running `redo build/html/<component_name>_tests.html`.

The first step in creating a unit test is to generate the required templates and copy them into the same directory as the unit test model. First we create and copy the main source code for running the test:

```
$ redo build/template/test.adb
$ cp build/template/* .
```

99% of the time you will not need to modify this file. It is a simple stub which is used to create and run the test harness through all of the configured tests.

Next, we build and copy the component tester templates.

```
$ redo build/template/component-<component_name>-implementation-tester.adb build/template/component-<component_name>-implementation-tester.ads
$ cp build/template/* .
```

These files encode the behavior of the component test harness. The default templates provide many good data structures for capturing test data, however these files may be modified to your heart's content to provide as rich of a testing environment as you desire. For simple components, the default templates will usually suffice.

Finally, we build the unit test templates and copy them:

```
$ redo build/template/<component_name>_tests-implementation.ads build/template/<component_name>_tests-implementation.ads
$ cp build/template/* .
```

These files contain the actual tests that are run during unit test. By default, the tests are empty and fail. You should fill in the implementation of your tests into these files. See unit testing examples in the `src/components` directory.

There are many other outputs that can be built by `redo` in a unit test directory. These files are needed to compile the unit test executable, but they usually don't need to be looked at by a developer. Their function is described below for completeness:

* `build/src/<component_name>_tests.ad[b,s]` - These are the auto generated unit test base class package files. These files contain test startup and teardown procedures and abstract test methods, forcing the user to implement them in the inheriting package.
* `build/src/<component_name>_tests-implementation-suite.ad[b,s]` - These are the auto generated unit tests suite files. This source organizes the test into a single suite for execution by `test.adb`.
* `build/src/component-<component_name>_reciprocal.ad[b,s]` - These files are the base class for the reciprocal testing component. They make up the structure of the tester component, and is the class from which the behavior `build/template/<component_name>_tests-implementation.ad[b,s]` class inherits.
* Object files in `build/obj` for all of the compiled source code mentioned in this section

With the testing environment implemented in `component-<component_name>-implementation-tester.ad[b,s]` and the unit tests written in `<component_name>_tests-implementation.ad[b,s]` we can now build and run the unit test. The unit test is built by running:

```
$ redo build/bin/Linux_Test/test.elf
```

and run using:

```
$ redo test
```

## Generating .gpr Project Files for GPS

GNAT Programming Studio (GPS) project files (.gpr) can be created for any executable in Adamant. This includes main executables and unit tests. See "Unit Testing a Component" or "Creating a Main Executable" for details. Note that as long as there is a `main.adb` or a `test.adb` file present in a directory, redo will allow you to generate a project file. For example in a directory with the contents:

```
$ ls
test.adb
```

when you run `redo what` you should see:

```
$ redo what
redo all
redo clean
redo build/bin/Linux/test.elf
redo build/gpr/test.gpr
redo build/obj/Linux/test.o
redo test
```

You can now build the `.gpr` file by running `redo build/gpr/test.gpr`. This project file will include all of the paths found in the current build path and can be readily opened with GNAT Programming Studio or another IDE of your choice.

## Adding Events to a Component

An event suite can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. Usually, event suites are created in the same directory as the component they are associated with. For example:

```
$ cd component-dir
$ touch <insert_component_name>.events.yaml
```

Usually, it will be easiest to start with an existing event suite model, and from there create your own. There are many examples in `src/components`. Before creating an event model, it is advisable to create any packed types that you might use as `param_type`s for the events. Note that it is essential to use packed types for event `param_types` since these data structures will be sent to the ground and should have a well-defined bit layout. See "Creating a Packed Type" for more details.

After creating the event model, we can run `redo what` to determine what we can build:

```
$ redo what
redo all
redo clean
redo build/html/<component_name>_events.html
redo build/obj/Linux/<component_name>_events-representation.o
redo build/obj/Linux/<component_name>_events.o
redo build/src/<component_name>_events-representation.adb
redo build/src/<component_name>_events-representation.ads
redo build/src/<component_name>_events.adb
redo build/src/<component_name>_events.ads
```

You can view an html table of your events by running `redo build/html/<component_name>_events.html`. The autogenerated source code for creating events can be found in `build/src/<component_name>_events.ad[b,s]`. The representation package, which allows you to print events in a human readable form is generated in `build/src/<component_name>_events-representation.ad[b,s]`. Each of these source code packages can be compiled into their corresponding objects using the `redo build/obj/..` commands above.

Now, you will be able to create events inside of your component using the `self.events` record element.


## Adding Data Products to a Component

A data product suite can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. Usually, data product suites are created in the same directory as the component they are associated with. For example:

```
$ cd component-dir
$ touch <insert_component_name>.data_products.yaml
```

Usually, it will be easiest to start with an existing data product suite model, and from there create your own. There are many examples in `src/components`. Before creating a data product model, it is advisable to create any packed types that you might use as `type`s for the data products. Note that it is essential to use packed types for data product `type`s since these data structures will be sent to the ground and should have a well-defined bit layout. See "Creating a Packed Type" for more details.

After creating the data product model, we can run `redo what` to determine what we can build:

```
$ redo what
redo all
redo clean
redo build/html/<component_name>_data_products.html
redo build/obj/Linux/<component_name>_data_products-representation.o
redo build/obj/Linux/<component_name>_data_products.o
redo build/src/<component_name>_data_products-representation.adb
redo build/src/<component_name>_data_products-representation.ads
redo build/src/<component_name>_data_products.adb
redo build/src/<component_name>_data_products.ads
```

You can view an html table of your data products by running `redo build/html/<component_name>_data_products.html`. The autogenerated source code for creating data products can be found in `build/src/<component_name>_data_products.ad[b,s]`. The representation package, which allows you to print data products in a human readable form is generated in `build/src/<component_name>_data_products-representation.ad[b,s]`. Each of these source code packages can be compiled into their corresponding objects using the `redo build/obj/..` commands above.

Now, you will be able to create data products inside of your component using the `self.data_products` record element.

## Adding Commands to a Component

A command suite can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. Usually, command suites are created in the same directory as the component they are associated with. For example:

```
$ cd component-dir
$ touch <insert_component_name>.commands.yaml
```

Usually, it will be easiest to start with an existing command suite model, and from there create your own. There are many examples in `src/components`. Before creating a command model, it is advisable to create any packed types that you might use as `arg_type`s for the commands. Note that it is essential to use packed types for command `arg_type`s since these data structures will be sent from the ground and should have a well-defined bit layout. See "Creating a Packed Type" for more details.

After creating the command model, we can run `redo what` to determine what we can build:

```
$ redo what
redo all
redo clean
redo build/html/<component_name>_commands.html
redo build/obj/Linux/<component_name>_commands.o
redo build/src/<component_name>_commands.adb
redo build/src/<component_name>_commands.ads
```

You can view an html table of your commands by running `redo build/html/<component_name>_commands.html`. The autogenerated source code for creating commands can be found in `build/src/<component_name>_commands.ad[b,s]`. This source code packages can be compiled into its corresponding objects using the `redo build/obj/Linux/<component_name>_commands.o` command.

Now, the autocoder will generate the code necessary to route commands to specific user-implemented handler functions. You can see these handlers if you generate the component template code following the instructions in the "Creating a Component" section.

## Adding Packets, Parameters, Faults, or Data Dependencies to a Component

The procedure for these is similar to adding Commands and Data Products, shown above. See User Guide for exact details.

## Creating an Assembly

An assembly can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. Usually, you will want to create an assembly in its own directory. To create an assembly, make a file of the name:

```
$ touch <insert_assembly_name>.assembly.yaml
```

Usually, it will be easiest to start with an existing assembly, and from there create your own. There are example assemblies in the Adamant Example repository in `src/assembly`. Before creating an assembly model, it is necessary to create models for any components you might use in that assembly. See "Creating a Component" for more details.

Next you can use your assembly model to generate many different output products. Simply run `redo what` in the same directory as the model file and you should get an output like:

```
$ redo what
redo all
redo clean
redo build/dot/<assembly_name>.dot
redo build/eps/<assembly_name>.eps
redo build/html/<assembly_name>_commands.html
redo build/html/<assembly_name>_components.html
redo build/html/<assembly_name>_connections.html
redo build/html/<assembly_name>_data_product.html
redo build/html/<assembly_name>_events.html
redo build/obj/Linux/<assembly_name>.o
redo build/png/<assembly_name>.png
redo build/src/<assembly_name>.adb
redo build/src/<assembly_name>.ads
redo build/svg/<assembly_name>.svg
redo build/xml/<assembly_name>.xml
```

From here you can generate the full assembly diagram by running `redo build/svg/<assembly_name>.svg`. You can then open this output in a web browser to view the diagram. ".png" and ".eps" versions of the diagram can also be built using the appropriate `redo` commands, as seen above.

Many html tables can also be generated for an assembly which outline the commands, components, connections, data products, and events included in the assembly. You can build these using the appropriate `redo` commands above. These files can readily opened and viewed in a web browser.

The assembly source code can be constructed by running `redo build/src/<assembly_name>.adb` and `redo build/src/<assembly_name>.ads`. This package contains the instances of all the components in the assembly and methods for connecting and initializing the components. These methods are generally called from a hand-written main function. See "Creating a Main Executable" for details. You can compile this code into an object by running `redo build/obj/Linux/<assembly_name>.o`.

## Creating Assembly Views

Assembly views can be constructed in any directory in the system that is in the build path. See "Adding to the Build Path" for more details. Usually, you will want to create assembly views in a subdirectory under the assembly model file that they are associated with. To create a view, make a file of the name:

```
$ mkdir -p view
$ cd view
```

From this directory, we can make a view by creating a file of the name:

```
$ touch <insert_view_name>.<insert_assembly_name>.view.yaml
```

Usually, it will be easiest to start with an existing view, and from there create your own. There are example views in the Adamant Example repository in `src/assembly/Linux/views`. Before creating an assembly view, it is necessary to create the assembly model it is associated with. See "Creating an Assembly" for more details.

Next you can use your assembly view model to generate a diagram of your view. Simply run `redo what` in the same directory as the model file and you should get an output like:

```
$ redo what
redo all
redo clean
redo build/dot/<view_name>.dot
redo build/eps/<view_name>.eps
redo build/png/<view_name>.png
redo build/svg/<view_name>.svg
```

From here you can generate the view diagram by running `redo build/svg/<view_name>.svg`. You can then open this output in a web browser to view the diagram. ".png" and ".eps" versions of the diagram can also be built using the appropriate `redo` commands, as seen above.

## Creating Assembly Documentation

Assembly documentation must be created in a `doc/` directory below the location of a assembly's model file. To create documentation you must first make this directory and `cd` into it:

```
$ mkdir -p doc
$ cd doc
```

From here you can run `redo what` to see what you can build.

```
$ redo what
redo all
redo clean
redo publish
redo build/pdf/<assembly_name>.pdf
redo build/template/<assembly_name>.tex
redo build/tex/<assembly_name>_commands.tex
redo build/tex/<assembly_name>_components.tex
redo build/tex/<assembly_name>_connections.tex
redo build/tex/<assembly_name>_data_products.tex
redo build/tex/<assembly_name>_description.tex
redo build/tex/<assembly_name>_events.tex
redo build/tex/<assembly_name>_stats.tex
redo build/tex/<assembly_name>_types.tex
redo build/tex/<assembly_name>_views.tex
```

From here you can build many latex (.tex) output files which are generated from the assembly's model. To build the assembly documentation (.pdf) you are first responsible for hand writing the latex master document file. A template for this file can be generated by running `redo build/template/<assembly_name>.tex`. This file should be copied into the `doc` directory by running:

```
$ cp build/template/* .
```

From here you can modify the `<assembly_name>.tex` file to your heart's content. However, the generated template is quite good, and might suffice for many simple assemblies. You can now build the assembly documentation by running `redo build/pdf/<assembly_name>.pdf`.

Note that anything in a `build/` directory is removed upon `redo clean` and will not be saved. "Published" documentation must be copied out of `build/pdf` and added to the version control system in a more permanent directory like `doc/`. To automatically build the documentation and copy it to this location, simply run `redo publish`.

## Creating a Main Executable

Main executables in Adamant can only be generated from a file named `main.adb`, and these files should never be put in the build path directly. Main source files should usually be created in their own directory:

```
$ mkdir -p main
$ cd main
```

From this directory, we need to create a main source file that is named `main.adb`:

```
$ touch main.adb
```

The build system will recognize the presence of a `main.adb` file and provide additional build outputs to support the compiling, linking, binding, and running of an executable. Running `redo what` produces:

```
$ redo what
redo all
redo clean
redo build/obj/Linux/main.o
redo build/gpr/main.gpr
redo build/obj/Linux/main.o
redo run
```

The main object can be constructed with `redo build/obj/Linux/main.o`. The main executable file can be constructed in ELF format with `build/obj/Linux/main.o`. A `.gpr` project file for use with an IDE can be generated for this main executable by running `redo build/gpr/main.gpr`. Finally, a special rule is generated called `redo run` which will run the executable from the command line after first compiling it.

The purpose of a main file in Adamant is to initialize the components and start all of the system tasks. This is done by importing the assembly package that contains all of the components you wish to run, and calling the methods of this package in sequence. An example of how this should be done is provided in the [Example Project](https://github.com/lasp/adamant_example) in `src/assembly/linux/main/main.adb`.
