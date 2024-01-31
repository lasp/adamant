from models.exceptions import ModelException, throw_exception_with_filename
from util import model_loader
from util import error as system_error
from util import ada
from util import shell
from util import jinja
from util import redo_arg
import os.path
import abc
import datetime
from database.model_cache_database import model_cache_database
from database.database import DATABASE_MODE
import database.model_database
import sys
import re
import textwrap

# This is the base object for generator models. All other generator
# models should inherit from this base. This class does a few things:
#
#   1) Provides some internal attributes that will be useful in most
#      child classes
#   2) When init is called, a yaml filename and a schema are passed in.
#      The yaml file is automatically validated against the schema, opened,
#      loaded into the python child model object, and then checked for validity.
#   3) This class provides reusable code to validate a yaml model against
#      its schema, open a yaml file for parsing, and rending a child class
#      into an output product using a Jinja template.
#   4) Provides abstract method load() which child classes must fill in.
#


# Helper functions:
def unique(lst):
    return list(set(lst))


# Print a multiline string with the provided prefix and wrapped over the
# maximum line length.
def printMultiLine(stringList, prefix="   ", max_line_length=79):
    # Remove trailing white spaces that appear right before endlines:
    def remove_trailing_spaces(string):
        return re.sub(r"\s+\n", "\n", string)

    # Preserve any embedded newlines by splitting into list of lines:
    stringList = stringList.split("\n")
    if stringList:
        # Wrap any lines over the max:
        wrapped_string_list = []
        for string in stringList:
            wrapped_string_list.extend(textwrap.wrap(string, max_line_length))
        return remove_trailing_spaces(
            prefix + ("\n" + prefix).join(wrapped_string_list)
        )
    return ""


#
# This function can be used in a Jinja template to print to the console via:
#  {{ debug_print("Hello, world!") }}
#
def debug_print(text):
    import sys

    sys.stderr.write(text + "\n")
    return ""


# A class that has a render method, to render via jinja using a provided template:
class renderable_object(object):
    def __new__(cls, *args, **kwargs):
        # return super(renderable_object, cls).__new__(cls, *args, **kwargs)
        obj = super(renderable_object, cls).__new__(cls)
        # Save off some local functions that are useful in jinja templates:
        obj.zip = zip
        obj.unique = unique
        obj.printMultiLine = printMultiLine
        return obj

    # Render this object using jinja and the provided template.
    # This method takes a provided template file and produces an output file
    # based on it, filling in its contents with members from the child class
    # object.
    def render(self, template_file, template_path=None):
        return jinja.render(
            self.__dict__, template_file, template_path, extensions=["jinja2.ext.do"]
        )


# The model base class. All python models that load yaml files should
# inherit from this class.
class base(renderable_object, metaclass=abc.ABCMeta):
    #################################################
    # Model Caching:
    #################################################

    def load_from_cache(cls, filename):
        def is_model_cached_this_session(filename):
            with model_cache_database() as db:
                # Get the time when we last cached the model from this file:
                cached_sesion_id = db.get_model_session_id(filename)
                if cached_sesion_id is not None and cached_sesion_id == os.environ["ADAMANT_SESSION_ID"]:
                    return True
            return False

        def is_cached_model_up_to_date(filename):
            # See if the model is stored in the database  cache stored on disk:
            with model_cache_database() as db:
                # Get the time when we last cached the model from this file:
                cache_time_stamp = db.get_model_time_stamp(filename)
                if cache_time_stamp is None:
                    return False

                # If the cache time is newer than the file modification time
                # then we can safely use the cached model:
                try:
                    file_time_stamp = os.path.getmtime(filename)
                except FileNotFoundError:
                    return False
                if cache_time_stamp >= file_time_stamp:
                    return True

        def do_load_from_cache(filename):
            with model_cache_database() as db:
                return db.get_model(filename)  # This can return None

        def mark_model_cached_this_session(filename):
            with model_cache_database(mode=DATABASE_MODE.READ_WRITE) as db:
                db.mark_cached_model_up_to_date_for_session(filename)

        # If the model was cached this redo session, then we know it is safe to use
        # directly from cache without any additional checking. Dependencies do not
        # need to be checked, since this would have been done earlier in this session,
        # ie. milliseconds ago.
        if is_model_cached_this_session(filename):
            return do_load_from_cache(filename)

        # If the model was written from a previous session, then we need to check its
        # write timestamp against the file timestamp to determine if the cached entry is
        # still valid:
        model = None
        if is_cached_model_up_to_date(filename):
            model = do_load_from_cache(filename)

        # We have a model we can use from cache. It was written in a previous
        # session. This is only safe to use if none of the model dependencies
        # have not changed on disk either.
        if model is not None:
            for dep_model_filename in model.get_dependencies():
                if not is_cached_model_up_to_date(dep_model_filename):
                    # One of the dependencies models has recently changed on disk,
                    # so we need to reload this model from scratch. We cannot safely
                    # use the cached version.
                    return None

        # This cached model has been fully validated for this session. So if we use it again
        # during the session, we can short circuit the full validation and return from
        # is_model_cached_this_session()
        mark_model_cached_this_session(filename)

        return model

    def save_to_cache(self):
        # Save model in the cache:
        with model_cache_database(mode=DATABASE_MODE.READ_WRITE) as db:
            db.store_model(self.full_filename, self)

    #################################################
    # Core class methods:
    #################################################

    # Create a model object. This constructor will look for a version
    # of this model that is already cached and return that, or if no
    # cached version is found, will return a new object.
    def __new__(cls, filename, *args, **kwargs):
        # Try to load the model from the cache:
        if filename:
            full_filename = os.path.abspath(filename)
            model = cls.load_from_cache(cls, full_filename)
            if model:
                # Create from cached model:
                self = model
                self.from_cache = True
                self.filename = os.path.basename(filename)
                self.full_filename = full_filename
                self.do_save_to_cache = True
                # import sys
                # sys.stderr.write("lcache " + self.filename + "\n")
            else:
                # Create from scratch:
                self = super(base, cls).__new__(cls)
                self.from_cache = False
                self.filename = os.path.basename(filename)
                self.full_filename = full_filename
                self.do_save_to_cache = True
                # import sys
                # sys.stderr.write("lfile " + self.filename + "\n")
        else:
            # Create from scratch. This is usually only called when
            # reconstructing the object from cache.
            self = super(base, cls).__new__(cls)
            self.do_save_to_cache = True
        return self

    # This function provides the "filename" argument to pickle so
    # that things get serialized correctly when pickle calls the
    # __new__ function above.
    def __getnewargs__(self):
        return (None,)

    # Initialize the base class. This includes validating the yaml file against
    # its schema, opening the yaml file, loading it into the child class object
    # and checking it for validity. Once this method finishes, you can be sure that
    # the Yaml file has been fully loaded into its corresponding python object, is
    # valid, and ready to use for output file generation.
    @throw_exception_with_filename
    def __init__(self, filename, schema):
        # If model was found in cache, then just use it, do not
        # initialize:
        if not self.from_cache:
            # Initialize object members:
            (
                self.model_name,
                self.model_type,
                self.specific_name,
            ) = database.model_database.split_model_file_name(filename)
            self.full_file_dir = os.path.dirname(self.full_filename)
            self.basename = os.path.basename(self.full_filename)
            self.full_schema = os.path.abspath(schema)
            self.schema = os.path.splitext(os.path.basename(self.full_schema))[0]
            self.time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")

            # Save off some local functions:
            self.zip = zip
            self.unique = unique
            self.printMultiLine = printMultiLine
            #
            # This function can be used in a Jinja template to print to the console via:
            #  {{ debug_print("Hello, world!") }}
            #
            self.debug_print = debug_print
            self.isTypePrimitive = ada.isTypePrimitive
            self.formatVariable = ada.formatVariable
            self.formatType = ada.formatType
            self.get_src_dir = redo_arg.get_src_dir

            # Load the object from the file:
            self.openYaml()
            self.validate()
            self.clean()
            self.load()

            # Save model in the cache:
            if self.do_save_to_cache:
                self.save_to_cache()

    # Method provided to support sets of these models. Note that
    # each python model corresponds uniquely to the file it was
    # loaded from. So we can hash on that.
    def __hash__(self):
        return hash(self.full_filename)

    # Method provided to support sets of these models. Note that
    # each python model corresponds uniquely to the file it was
    # loaded from. So we can compare via that.
    def __eq__(self, other):
        return self and other and self.full_filename == other.full_filename

    # String representation of model class
    def __repr__(self):
        return "<class " + self.__class__.__name__ + "(" + self.basename + ")>"

    def __str__(self):
        return self.__repr__()

    # Validate that the yaml file is valid yaml and run the linter to make sure it
    # looks good, conforming to our linter configuration:
    def lint(self):
        self._lint(self.full_filename)

    # Validate the yaml file against a schema to make sure it is formatted correctly.
    def validate(self):
        if self.full_schema:
            # Pykwalify requires a the yaml be stored in a file prior to validation. We may
            # have altered the contents of the yaml based on the adamant global configuration
            # variables. So save the (possibly modified) file contents to a temp file, and then
            # pass that into Pykwalify.
            import os
            import tempfile

            temp_fd, temp_file = tempfile.mkstemp(suffix=".yaml")
            try:
                # Create an object and validate against the schema
                from pykwalify.core import Core

                with os.fdopen(temp_fd, "w") as tmp:
                    # Write model file contents to temp file
                    tmp.write(self.file_contents)
                # Perform Pykwalify model validation against the schema
                c = Core(source_file=temp_file, schema_files=[self.full_schema])
                c.validate(raise_exception=True)
            except BaseException as e:
                raise ModelException(
                    "Error occurred while validating "
                    + self.full_filename
                    + " against "
                    + self.full_schema
                    + ".\n"
                    + str(e)
                    + "\n"
                )
            finally:
                # Make sure we clean up the temp file.
                os.remove(temp_file)

    # Open the yaml file, parse it, and store its contents in self.data:
    def openYaml(self):
        # Helper function to load yaml using ruamel.yaml library:
        def _loadYaml(yaml_text):
            import ruamel.yaml as yaml

            # Turn off warnings for unsafe yaml loading. We don't care.
            import warnings

            warnings.simplefilter("ignore", yaml.error.UnsafeLoaderWarning)
            # with open(self.full_filename, 'r') as stream:
            try:
                yml = yaml.YAML(typ='rt')
                return yml.load(yaml_text)
                # import sys
                # sys.stderr.write(str(self.data) + "\n")
                # sys.stderr.write(str(type(self.data)) + "\n")
            except yaml.YAMLError as exc:
                raise ModelException(str(exc))

        # If there are any Jinja variables within the yaml file, then
        # fill those in using the global Adamant config:
        # Only do this substitution if the yaml file we are loading is
        # not an Adamant configuration file.
        resolved_yaml = None
        if self.model_type != "configuration":
            self.config = model_loader.load_project_configuration()
            resolved_yaml = self.config.render(self.full_filename, template_path=os.sep)
        else:
            with open(self.full_filename, "r") as f:
                resolved_yaml = f.read()

        # Save the data within the object:
        self.file_contents = resolved_yaml
        self.data = _loadYaml(resolved_yaml)

    # Method for sanitizing the self.data contents prior to calling load:
    def clean(self):
        # Recursive function that strips all value strings:
        def _strip_values(item):
            if isinstance(item, str):
                return item.strip(" \t\n")
            elif isinstance(item, dict):
                for k, v in item.items():
                    item[k] = _strip_values(v)
            elif isinstance(item, list):
                item = [_strip_values(i) for i in item]
            return item

        # Call recursive strip function:
        self.data = _strip_values(self.data)

    # Function which returns the dependencies of the model. This is useful information
    # for redo, so that if dependencies for a certain model change, then all autocode
    # using that model can be rebuilt. By default, if this method is not overridden, then
    # an empty list of dependencies is returned.
    def get_dependencies(self):
        return []

    # Abstract method for load. Child classes should override this method and
    # load data from self.data into object specific data structures that will be
    # useful for file generation with the templates. It is also recommended
    # that you perform a check of the input YAML file data to ensure that it is
    # valid. Schema validation does a good job at checking most issues with
    # YAML files, however, there is certain things that a schema just cannot
    # check. Use this method to check those things.
    @abc.abstractmethod
    def load(self):
        pass

    # def render(self, template_file, template_path=None):
    # ^ Now inherited from renderable_object class

    def _error_str(self, string, line_number=None):
        return (
            self.full_filename
            + ":"
            + ((str(line_number + 1) + ":") if line_number else "")
            + " "
            + string
        )

    def warning(self, string, line_number=None):
        system_error.warning_print("Warning " + self._error_str(string, line_number))

    # Warning message for model:
    def warn(self, string, line_number=None):
        sys.stderr.write("Warning " + self._error_str(string, line_number) + "\n")

    # Helper function to get file paths:
    def get_path_from(self, path_from):
        # Return the following:
        #  relative path to this model file from the provided path
        return os.path.relpath(self.full_filename, path_from)

    def get_dir_from(self, path_from):
        # Return the following:
        #  relative path to this model dir from the provided path
        this_rel_path = self.get_path_from(path_from)
        return os.path.normpath(os.path.dirname(this_rel_path))

    def get_src_dir_from(self, path_from):
        # Return the following:
        #  relative source path to this model dir from the provided path
        # ie. This is the same as get_dir_from except that if the result
        # is in a build directory, it returns the root directory where that
        # build directory lives.
        return redo_arg.get_src_dir(self.get_path_from(path_from))

    def get_path_to(self, path_to):
        # Return the following:
        #  relative path to the provided model file from this model file
        return os.path.relpath(path_to, self.full_file_dir)

    def get_dir_to(self, path_to):
        # Return the following:
        #  relative path to the provided model dir from this model file
        this_rel_path = self.get_path_to(path_to)
        return os.path.normpath(os.path.dirname(this_rel_path))

    def get_src_dir_to(self, path_to):
        # Return the following:
        #  relative path to the provided model dir from this model file
        # ie. This is the same as get_dir_to except that if the result
        # is in a build directory, it returns the root directory where that
        # build directory lives.
        return redo_arg.get_src_dir(self.get_path_to(path_to))

    #################################################
    # Private methods:
    #################################################

    def _lint(self, filename, quit_on_error=True):
        cmd = (
            "yamllint -c "
            + os.environ["SCHEMAPATH"]
            + "/yaml_lint_config.yaml "
            + filename
            + "> /dev/stderr"
        )
        if not quit_on_error:
            cmd += " | true"
        shell.run_command(cmd)
