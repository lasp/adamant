import os.path
import sys
from util import error
from util import redo_arg
from util import target
from util import meta
from util import ada
import re
import inspect

# Database imports:
from database.database import DATABASE_MODE
from database.source_database import source_database
from database.c_source_database import c_source_database
from database.py_source_database import py_source_database
from database.generator_database import generator_database
from database.utility_database import utility_database
from database.redo_target_database import redo_target_database
from database.build_target_database import build_target_database
import database.model_cache_database
from database.model_database import model_database

# Base class imports:
from base_classes.build_target_base import build_target_base
from base_classes.gprbuild_target_base import gprbuild_target_base
from base_classes.generator_base import generator_base
import base_classes.build_rule_base

# Import generators, rules, and build targets from
# known locations. This means that we get to load less
# modules dynamically in the create() function, speeding
# up the create() function significantly.
# from generators import *
# from rules import *
# from targets import *

# OK, lets do the above manually, instead since we have
# deleted all the magic __init__.py logic:
# import glob
# import os.path
# import importlib
# base_dir = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
# modules = glob.glob(base_dir + "/redo/rules/*.py")
# modules = [os.path.basename(f)[:-3] for f in modules if os.path.isfile(f) and not f.endswith('__init__.py')]
# for m in modules:
#   importlib.import_module("rules." + m)
# modules = glob.glob(base_dir + "/redo/targets/*.py")
# modules = [os.path.basename(f)[:-3] for f in modules if os.path.isfile(f) and not f.endswith('__init__.py')]
# for m in modules:
#   importlib.import_module("targets." + m)
# modules = glob.glob(base_dir + "/gen/generators/*.py")
# modules = [os.path.basename(f)[:-3] for f in modules if os.path.isfile(f) and not f.endswith('__init__.py')]
# for m in modules:
#   importlib.import_module("generators." + m)

#
# ^^
# No longer need to do the above. We just load the modules
# dynamically, instead of using the old __init__.py magic.
# It makes the create() function slower, but
# speeds up the build system as a whole, significantly
# reducing python startup (import) time.
#

# This module provides a single public function called
# "create()" which creates all the build system databases.


#####################################################
# Private functions:
#####################################################
# Given a list of strings, and a compiled regex object
# return only strings in the list that the regex is able to match.
def _filter_by_regex(compiled_regex, list_of_strings):
    return (string for string in list_of_strings if compiled_regex.match(string))


# Add to dictionary that maps a regex string to a tuple of
# the form:
#
# (compiled_regex, {object_instance1, object_instance2, etc.}
#
# This datastructure helps us only compile regex strings once
# per unique regex string.
def _add_to_regex_dict_of_sets(dic, regex, object_instance):
    if regex in dic:
        cregex, oi_set = dic[regex]
        oi_set.add(object_instance)
    else:
        cregex = re.compile(regex)
        dic[regex] = (cregex, {object_instance})


# Similar to above but add a regex + ".do"
def _add_to_regex_dict_of_sets_w_do(dic, regex, object_instance):
    if regex in dic:
        cregex, do_cregex, oi_set = dic[regex]
        oi_set.add(object_instance)
    else:
        cregex = re.compile(regex)
        regex_prefix = regex
        if regex[-1] == "$":
            regex_prefix = regex[:-1]
        do_cregex = re.compile(regex_prefix + r"\.do$")
        dic[regex] = (cregex, do_cregex, {object_instance})


# Find all the generators that have the generator_base class
# as their parent, and return a dictionary that maps regex strings to
# instances of the generator classes that use those regex strings.
def _create_generator_regex_dict():
    regex_dict = dict()
    # First find all the generators classes that have been imported:
    generator_classes = meta.get_all_subclasses_of(generator_base)

    # For each generator create a dictionary that maps file regexes
    # to generator objects ie:
    #
    # "*.component.yaml" : {generator_object1, generator_object2, etc.}
    #
    # Where each generator object takes a file that matches the file
    # regex as input.
    for generator_class in generator_classes:
        generator_instance = generator_class()
        regex_string = generator_instance.input_file_regex()
        _add_to_regex_dict_of_sets(regex_dict, regex_string, generator_instance)
    return regex_dict


# Find all the rules that have the rule_base base class
# as their parent, and return a dictionary that maps regex strings to
# instances of the rule classes that use those regex strings.
def _create_rule_regex_dict():
    rule_dict = dict()
    # First find all the rule classes that have been imported:
    rule_classes = meta.get_all_subclasses_of(
        base_classes.build_rule_base.build_rule_base
    )

    # For each rule create a dictionary that maps file regexes
    # to rule objects ie:
    #
    # "*.ad[b,s]" : {rule_object1, rule_object2, etc.}
    #
    # Where each generator object takes a file that matches the file
    # regex as input.
    for rule_class in rule_classes:
        rule_instance = rule_class()
        rule_regexes = rule_instance.input_file_regex()
        # rule_regexes may not exist for a rule, and that is ok.
        if rule_regexes:
            if isinstance(rule_regexes, str):
                rule_regexes = [rule_regexes]
            for regex_string in rule_regexes:
                _add_to_regex_dict_of_sets_w_do(rule_dict, regex_string, rule_instance)
    return rule_dict


# Find all the targets that have the target_base base class
# as their parent, and return a dictionary that maps the target
# string name to instances of the target class
def _create_target_name_dict():
    target_dict = dict()
    # First find all the target classes that have been imported:
    target_classes = meta.get_all_subclasses_of(build_target_base)
    target_classes.extend(meta.get_all_subclasses_of(gprbuild_target_base))

    # For each target create a dictionary the target name to
    # to information to generate an instance of that target:
    #
    # "Linux" : (modulename, filename)
    #
    for target_class in target_classes:
        module_name = target_class.__module__
        class_name = target_class.__name__
        filename = sys.modules[module_name].__file__
        target_dict[class_name] = (module_name, filename)
    return target_dict


# Print an error to the user when the output filename for a
# generator or rule did not work correctly.
def _output_filename_error(exception, object_instance, input_filename):
    import traceback

    error.error_print(
        "Error encountered while running '"
        + object_instance.__module__
        + "."
        + object_instance.output_filename.__self__.__class__.__name__
        + "."
        + object_instance.output_filename.__name__
        + "("
        + input_filename
        + ")': "
        + str(exception)
    )
    exc_type, exc_value, exc_traceback = sys.exc_info()
    traceback.print_exception(exc_type, exc_value, exc_traceback, file=sys.stderr)
    error.abort()


# Format an output filename, making sure it is of absolute path.
def _format_output_filename(output_filename, object_instance):
    assert os.path.isabs(output_filename), (
        "Output file names produced by '"
        + str(object_instance.__class__)
        + "' must be absolute paths. The following path is not absolute: "
        + str(output_filename)
    )
    return output_filename


# Determine if a filename is an yaml model file by its extension.
def _is_yaml_source_file(filename):
    dirname, specific_name, model_name, model_type, ext = redo_arg.split_model_filename(
        filename
    )
    return model_name and model_type


# Determine if a filename is an ada source file by its extension.
def _is_ada_source_file(filename):
    _, ext = os.path.splitext(filename)
    return ext in [".adb", ".ads"]


# Determine if a filename is c/c++ or asm source file by its extension.
def _is_c_source_file(filename):
    _, ext = os.path.splitext(filename)
    return ext in [".c", ".cpp", ".h", ".hpp", ".s"]


# Determine if a filename is python source file by its extension.
def _is_py_source_file(filename):
    _, ext = os.path.splitext(filename)
    return ext == ".py"


#####################################################
# Private classes:
#####################################################
# This class extends a dictionary that maps all the
# directories in the build path to a set of build
# rules that are available for that directory, ie.
#
# "/path/to/dir" = {"build/src/source.adb", "build/obj/source.o", etc.}
#
class _redo_target_dictionary(dict):
    # Create the dictionary:
    def __init__(self):
        pass

    # Add a target to the dictionary for a certain
    # directory.
    def add_target(self, directory, target):
        # Add the value to the set associated with
        # the dictionary key.
        def _add_to_dict_of_sets(dic, key, value):
            try:
                dic[key].add(value)
            except Exception:
                dic[key] = {value}

        # Add the target for this directory:
        _add_to_dict_of_sets(self, directory, target)

        # If the targets source dir is a subdirectory of
        # directory, add all this target for
        # all paths in between:
        target_dir = redo_arg.get_src_dir(target)
        if directory != target_dir and target_dir.startswith(directory):
            while directory != target_dir:
                _add_to_dict_of_sets(self, target_dir, target)
                target_dir = os.path.dirname(target_dir)


#####################################################
# Public functions:
#####################################################


# Create new build system databases that do not depend on
# the build path. We split this up because this operation can
# be run even before the build path is calculated, which is
# necessary for some operations.
def create_pre_build_path():
    #######################################
    # Dynamic module import:
    #######################################
    # Find all modules in the python path that could be considered:
    #   - generators
    #   - build rules
    #   - build targets
    #
    # and import them.
    modules = meta.get_modules_in_dir(
        ["generator", "generators", "target", "targets", "rule", "rules"]
    )
    for module_name, filename in modules:
        # sys.stderr.write(str([module_name, filename]) + "\n")
        meta.import_module_from_filename(filename, module_name)

    #######################################
    # Create useful dictionaries:
    #######################################
    target_name_dict = _create_target_name_dict()

    #######################################
    # Create cache databases:
    #######################################
    # Create an empty model cache database if it doesn't already exist:
    database.model_cache_database.touch_model_cache_database()

    #######################################
    # Create build target database:
    #######################################
    # This database maps build target names, like "Linux"
    # to actual instances of those build target objects.
    with build_target_database(mode=DATABASE_MODE.CREATE) as build_target_db:
        for key, values in target_name_dict.items():
            build_target_db.insert_build_target_instance(key, *values)


# Create new build system databases from the build path:
# The build_path is a dictionary object that maps each
# directory in the build path to all the files
# found in that directory.
def create(build_path):
    # The redo target dictionary objects for storing
    # the redo targets that exist for each directory
    # in the build path:
    redo_target_dict = _redo_target_dictionary()

    # Make a copy of the current path
    source_build_path = list(build_path.keys())

    #######################################
    # Create useful dictionaries:
    #######################################
    generator_regex_dict = _create_generator_regex_dict()
    rule_regex_dict = _create_rule_regex_dict()

    #######################################
    # Source and generator database load:
    #######################################
    # Useful compiled regexes:
    ada_source_regex = re.compile(r".*\.ad[sb]$")
    do_file_regex = re.compile(r".*\.do$")
    yaml_file_regex = re.compile(r".*\.yaml$")
    c_source_regex = re.compile(r".*\.(h|hpp|c|cpp|s)$")

    # Search first for model files, and create the model database. This
    # may be needed by generators, so we need to build it first.
    with model_database(mode=DATABASE_MODE.CREATE) as model_db:
        # Iterate through every directory in the build path:
        model_files = []
        for directory, files in build_path.items():
            # Search for model files and add them to the model files
            # list:
            for filename in _filter_by_regex(yaml_file_regex, files):
                model_files.append(filename)
                model_db.insert_model(filename)

    # Open all the source and generator databases that will be populated
    # when we iterate through the build path.
    with source_database(mode=DATABASE_MODE.CREATE) as source_db, c_source_database(
        mode=DATABASE_MODE.CREATE
    ) as c_source_db, py_source_database(
        mode=DATABASE_MODE.CREATE
    ) as py_source_db, generator_database(
        mode=DATABASE_MODE.CREATE
    ) as generator_db:
        # Using the generator regexes, add any autogenerated source
        # to the source database. Add output_file -> generator map
        # to the generator database.
        def compute_generated_files(all_input_files):
            def _compute_generated_files(input_files):
                generated_models = []
                for regex_string, (cregex, generators) in generator_regex_dict.items():
                    for input_filename in _filter_by_regex(cregex, input_files):
                        for generator in generators:
                            # Grab the output filename from the generator:
                            try:
                                output_filename = generator.output_filename_(
                                    input_filename
                                )
                            except Exception as e:
                                _output_filename_error(e, generator, input_filename)

                            # If the output name is valid, add it to the generator database and
                            # the redo target dictionary:
                            if output_filename:
                                output_filename = _format_output_filename(
                                    output_filename, generator
                                )
                                generator_db.insert_generator(
                                    output_filename,
                                    input_filename,
                                    generator.__module__,
                                    generator.generate.__self__.__class__.__name__,
                                    inspect.getfile(generator.__class__),
                                )
                                redo_target_dict.add_target(directory, output_filename)

                                # Make sure that this source file is actually in the build path. This
                                # if statement prevents things like autogenerated files found in "template",
                                # which are not in the build path, from being added to this dictionary:
                                output_dir = os.path.dirname(output_filename)
                                output_dir_name = os.path.basename(output_dir)
                                if output_dir_name == "src":
                                    if _is_ada_source_file(output_filename):
                                        source_db.insert_source(
                                            output_filename, input_filename
                                        )
                                    if _is_c_source_file(output_filename):
                                        c_source_db.insert_source(
                                            output_filename, input_filename
                                        )
                                    # Add this directory to the source build path:
                                    source_build_path.append(output_dir)
                                elif output_dir_name == "py":
                                    if _is_py_source_file(output_filename):
                                        py_source_db.insert_source(
                                            output_filename, input_filename
                                        )
                                # If the output file of the generator is yaml, then add it do the models
                                # dictionary:
                                elif output_dir_name == "yaml":
                                    if _is_yaml_source_file(output_filename):
                                        generated_models.append(output_filename)

                # Add all generated models to the model database, and return them:
                with model_database(mode=DATABASE_MODE.READ_WRITE) as model_db:
                    for model_file in generated_models:
                        model_db.insert_model(model_file)
                return generated_models

            # Continue to run this function on all generated files until there is
            # no more generated files found.
            next_input_files = all_input_files
            while next_input_files:
                next_input_files = _compute_generated_files(
                    input_files=next_input_files
                )

        # Iterate through every directory in the build path:
        for directory, files in build_path.items():
            # sys.stderr.write(directory + " " + str(files) + "\n")

            # Add existing Ada source files to database:
            for filename in _filter_by_regex(ada_source_regex, files):
                source_db.insert_source(filename)

            # Add existing C and C++ source files to database:
            for filename in _filter_by_regex(c_source_regex, files):
                c_source_db.insert_source(filename)

            # Find any special .do files. Ignore default .do files:
            for filename in _filter_by_regex(do_file_regex, files):
                # If it is a default.do ignore it.
                basename = os.path.basename(filename)
                if not basename.startswith("default."):
                    do_target = filename[:-3]
                    # Add the do target to the redo_targets:
                    redo_target_dict.add_target(directory, do_target)
                    # If this do file builds an source file add it to the
                    # source database if it is not already in the source database
                    if _is_ada_source_file(
                        do_target
                    ) and do_target not in source_db.try_get_source(
                        ada.file_name_to_package_name(do_target)
                    ):
                        source_db.insert_source(do_target)
                    if _is_c_source_file(
                        do_target
                    ) and do_target not in c_source_db.try_get_source(
                        redo_arg.get_base_no_ext(do_target)
                    ):
                        c_source_db.insert_source(do_target)
                    if _is_py_source_file(
                        do_target
                    ) and do_target not in py_source_db.try_get_source(
                        redo_arg.get_base_no_ext(do_target)
                    ):
                        py_source_db.insert_source(do_target)

            # Using the generator regexes, add any autogenerated source
            # to the source database. Add output_file -> generator map
            # to the generator database.
            compute_generated_files(files)

            # Using the rule regexes, add any autogenerated files to the
            # redo target dictionary:
            for regex_string, (cregex, do_cregex, rules) in rule_regex_dict.items():
                for input_filename in _filter_by_regex(cregex, files):
                    for rule in rules:
                        # Grab the output filename from the rule:
                        try:
                            output_filename = rule.output_filename(input_filename)
                        except Exception as e:
                            _output_filename_error(e, rule, input_filename)

                        if output_filename:
                            output_filename = _format_output_filename(
                                output_filename, rule
                            )
                            redo_target_dict.add_target(directory, output_filename)

                # Using the rule regexes applied to special .do files, add any
                # autogenerated files to the redo target dictionary:
                for input_filename in _filter_by_regex(do_cregex, files):
                    for rule in rules:
                        if not basename.startswith("default."):
                            input_filename = input_filename[:-3]
                            # Grab the output filename from the rule:
                            try:
                                output_filename = rule.output_filename(input_filename)
                            except Exception as e:
                                _output_filename_error(e, rule, input_filename)

                            if output_filename:
                                output_filename = _format_output_filename(
                                    output_filename, rule
                                )
                                redo_target_dict.add_target(directory, output_filename)

    #######################################
    # Utility database load:
    #######################################
    # Form the source build path and the object build path:
    source_build_path = list(set(source_build_path))
    build_target = target.get_default_target()
    object_build_path = [
        os.path.join(path, "build" + os.sep + "obj" + os.sep + build_target)
        for path in build_path
    ]
    # Load some items into the utility database for fetching later:
    with utility_database(mode=DATABASE_MODE.CREATE) as utility_db:
        utility_db.store("build_path", build_path)
        utility_db.store("source_build_path", source_build_path)
        utility_db.store("object_build_path", object_build_path)
        utility_db.store("model_files", model_files)

    #######################################
    # Target database load:
    #######################################
    # We have populated the redo target dictionary in the code above. Now
    # we need to load it into the redo target database. Before we do that,
    # Let's inspect the redo targets for each directory, and see if there
    # are any rules that apply that can produce more targets for that
    # directory. This allows "redo what" and "redo all" to induce up to two
    # levels of possible redo targets. ie. if we know that we have a rule
    # to produce a .dot file, then we can inductively determine that we can
    # also create a .svg file from that .dot file.
    with redo_target_database(mode=DATABASE_MODE.CREATE) as redo_target_db:
        for directory, targets in redo_target_dict.items():
            # sys.stderr.write(directory + " " + str(targets) + "\n\n")
            # For each directory, see if we can add any more targets:
            targets_to_add = []
            for regex_string, (cregex, do_cregex, rules) in rule_regex_dict.items():
                for input_filename in _filter_by_regex(cregex, targets):
                    for rule in rules:
                        # Grab the output filename from the rule:
                        try:
                            output_filename = rule.output_filename(input_filename)
                        except Exception as e:
                            _output_filename_error(e, rule, input_filename)

                        if output_filename:
                            output_filename = _format_output_filename(
                                output_filename, rule
                            )
                            targets_to_add.append(output_filename)

            # Store the redo targets for this directory into the database:
            redo_target_db.store(directory, targets | set(targets_to_add))
