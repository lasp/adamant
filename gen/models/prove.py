from models.base import base
import os


# This model stores yaml information for configuring GNATprove for a directory.
class prove(base):
    def __init__(self, filename, template=os.environ["SCHEMAPATH"] + "/prove.yaml"):
        # Load the object from the file:
        super(prove, self).__init__(filename, template)

    # Load component specific data structures with information from YAML file.
    def load(self):
        self.description = None
        if "description" in self.data:
            self.description = self.data["description"]
        self.mode = None
        if "mode" in self.data:
            self.mode = self.data["mode"]
        self.level = None
        if "level" in self.data:
            self.level = self.data["level"]
