import os.path
from models import sequence_store
from base_classes.generator_base import generator_base
from generators.basic import basic_generator, add_basic_generators_to_module
from models import sequence_store_packets
from generators.ided_suite import packet_templates

# Add all the basic generators for normal packets, except use the sequence_store_packets model.
# This allows us to build all the normal packet outputs like html, tex, etc.
add_basic_generators_to_module(
    sequence_store_packets.sequence_store_packets, packet_templates, module=globals()
)


class sequence_store_ads(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=sequence_store.sequence_store,
            template_filename="name.ads",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (name).join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        p = sequence_store.sequence_store(input_filename)
        print(p.render(self.template, template_path=self.template_dir))


class sequence_store_yaml(basic_generator, generator_base):
    def __init__(self):
        this_file_dir = os.path.dirname(os.path.realpath(__file__))
        template_dir = os.path.join(this_file_dir, ".." + os.sep + "templates")
        basic_generator.__init__(
            self,
            model_class=sequence_store.sequence_store,
            template_filename="name_record.record.yaml",
            template_dir=template_dir,
        )

    def output_filename(self, input_filename):
        # Get info from input filename:
        dirname, specific_name, name, *ignore = self._split_input_filename(
            input_filename
        )

        if not specific_name:
            build_dir = self._get_default_build_dir()

            # Construct the filename:
            a = self.template_basename.rsplit("name", maxsplit=1)
            output_filename = (name).join(a)

            # Return the suggested output filename:
            return dirname + os.sep + build_dir + os.sep + output_filename
        return basic_generator.output_filename(self, input_filename)

    def generate(self, input_filename):
        p = sequence_store.sequence_store(input_filename)
        print(p.render(self.template, template_path=self.template_dir))
