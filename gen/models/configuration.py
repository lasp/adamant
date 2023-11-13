from models.base import base
from os import environ


# This is the object model for an Adamant configuration. The variables
# in this file are used to populate any jinja tags appearing in other
# YAML files throughout the system.
class configuration(base):
    # Initialize the packed array object, ingest data, and check it by
    # calling the base class init function.
    def __init__(self, filename):
        # Load it:
        super(configuration, self).__init__(
            filename, schema=environ["SCHEMAPATH"] + "/configuration.yaml"
        )

    # Nothing to do here...
    def load(self):
        # Load all the keys as member variables:
        self.__dict__.update(self.data)
