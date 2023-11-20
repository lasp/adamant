import os.path
from base_classes.generator_base import generator_base
from generators.assembly import assembly_generator


class event_to_text_ads(assembly_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self, "name_event_to_text.ads", template_dir=template_dir
        )


class event_to_text_adb(assembly_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        assembly_generator.__init__(
            self, "name_event_to_text.adb", template_dir=template_dir
        )
