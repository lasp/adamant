import os.path
import sys
from base_classes.build_rule_base import build_rule_base
from util import redo_arg
from database.model_database import model_database
from bin import yaml_sloc_counter


class build_yaml_sloc(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        sys.stderr.write(
            "Note, please run this after building your project main.elf to accurately calculate autocoded yaml.\n"
        )
        sys.stderr.write("Fetching all YAML in build path...\n")

        with model_database() as model_db:
            yaml_files = model_db.get_all_models()

        # Filter out files that do not exist:
        yaml_files = [f for f in yaml_files if os.path.exists(f)]

        # Determine which files are autocoded:
        autocoded_yaml_files = list(filter(redo_arg.in_build_dir, yaml_files))

        sys.stderr.write(
            "Calculating the YAML SLOC for " + str(len(yaml_files)) + " files...\n"
        )
        autocoded_sloc = yaml_sloc_counter.countSLOC(autocoded_yaml_files)
        total_sloc = yaml_sloc_counter.countSLOC(yaml_files)
        sys.stderr.write("Hand-coded SLOC: " + str(total_sloc - autocoded_sloc) + "\n")
        sys.stderr.write("Auto-coded SLOC: " + str(autocoded_sloc) + "\n")
        sys.stderr.write("Total SLOC:      " + str(total_sloc) + "\n")

    # No need to provide these for "redo yaml_sloc"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
