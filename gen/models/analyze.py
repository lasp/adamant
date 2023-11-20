from models.base import base
import os


# This model stores yaml information for configuring CodepEer for a directory.
class analyze(base):
    def __init__(self, filename, template=os.environ["SCHEMAPATH"] + "/analyze.yaml"):
        # Load the object from the file:
        super(analyze, self).__init__(filename, template)

    # Load component specific data structures with information from YAML file.
    def load(self):
        self.description = None
        if "description" in self.data:
            self.description = self.data["description"]
        self.messages = None
        if "messages" in self.data:
            self.messages = self.data["messages"]
        self.level = None
        if "level" in self.data:
            self.level = self.data["level"]
        self.switches = []
        if "switches" in self.data:
            self.switches = self.data["switches"]
