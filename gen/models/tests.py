from util import ada
from collections import OrderedDict
from models.exceptions import ModelException
from models.component import component_submodel
import os


# A class which holds a single unit test:
class test(object):
    def __init__(self, name, description=None):
        self.name = ada.formatType(name)
        self.description = description


# A class which holds a unit test suite:
class tests(component_submodel):
    # Initialize the unit test object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load the object from the file:
        super(tests, self).__init__(filename, os.environ["SCHEMAPATH"] + "/tests.yaml")

    # Load unit test specific data structures with information from YAML file.
    def load(self):
        # Call the component submodel load:
        component_submodel.load(self)

        # Initialize object members:
        self.name = None
        self.includes = []
        self.preamble = None
        self.description = None
        self.tests = OrderedDict()

        # Populate the object with the contents of the
        # file data:
        if "with" in self.data and self.data["with"]:
            self.includes = self.data["with"]
        for include in self.includes:
            include = ada.formatType(include)
        if self.specific_name:
            self.name = ada.formatType(self.specific_name)
        else:
            self.name = ada.adaCamelCase(self.model_name) + "_Tests"

        if "description" in self.data:
            self.description = self.data["description"]
        if "preamble" in self.data:
            self.preamble = self.data["preamble"]
        the_tests = self.data["tests"]
        for t in the_tests:
            name = ada.formatType(t["name"])
            description = None
            if "description" in t:
                description = t["description"]
            test_obj = test(name, description)
            if test_obj.name in self.tests:
                raise ModelException(
                    "Test names must be unique. Found duplicate test name: '"
                    + str(test_obj.name)
                    + "'"
                )
            else:
                self.tests[test_obj.name] = test_obj

    def __nonzero__(self):
        return bool(self.tests)

    def __iter__(self):
        return iter(self.tests.values())

    def __len__(self):
        return len(self.tests)

    # This unit test model may or may not have a component associated with it. So
    # we override this function to allow for the case that a component could not
    # be loaded. In that case, this is just a normal unit test, not a component
    # unit test.
    def load_component(self):
        try:
            # Do base class load:
            component_submodel.load_component(self)
        except ModelException:
            # If this occurred, then this unit test is likely not a component
            # unit test, so just continue on. We want self.component to be None
            # in this case.
            pass
