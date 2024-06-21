from models.component import component_submodel
from util import ada
import os.path


class requirements(component_submodel):
    """
    This is the object model for a data_products suite. It extracts data from a
    input file and stores the data as object member variables.
    """
    def __init__(self, filename):
        """
        This is the object model for an requirements suite. It extracts data from a
        input file and stores the data as object member variables.
        """
        # Load the object from the file:
        super(requirements, self).__init__(
            filename, os.environ["SCHEMAPATH"] + "/requirement.yaml"
        )

    def load(self):
        """Load requirements specific data structures with information from YAML file."""
        # Call the component submodel load:
        component_submodel.load(self)

        # Initialize object members:
        self.includes = []
        self.rep_includes = []
        self.name = None
        self.description = None
        self.localIdList = []
        # set of requirements, each should be a dictionary
        self.requirements = None

        # Populate the object with the contents of the
        # file data:
        if self.specific_name:
            self.name = ada.adaCamelCase(self.specific_name)
        else:
            self.name = ada.adaCamelCase(self.model_name) + "_Requirements"
        if "description" in self.data:
            self.description = self.data["description"]
        self.requirements = self.data["requirements"]

    def load_tests(self):
        """
        Load test tables, load mapping from requirements unit tests, validate them,
        associate reqs with their tests in its respective ut model
        """
        # for each req
        # if there are tests
        # load model
        # iterate model
        # if test is associated with file, add to all tests
        # if test is associated with test, add to that test
        # if file or test does not exist, throw an error
        # done
        pass
