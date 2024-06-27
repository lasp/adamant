#!/usr/bin/env python3
import argparse
from collections import OrderedDict
import re


def _openYaml(yaml_file):
    """Open the yaml file, parse it, and store its contents in self.data."""
    def _loadYaml(yaml_text):
        """Helper function to load yaml using ruamel.yaml library."""
        import ruamel.yaml as yaml

        # Turn off warnings for unsafe yaml loading. We don't care.
        import warnings

        warnings.simplefilter("ignore", yaml.error.UnsafeLoaderWarning)
        yml = yaml.YAML(typ='rt')
        return yml.load(yaml_text)

    # Open and read the yaml file:
    with open(yaml_file, "r") as f:
        yaml_text = f.read()

    # Remove any jinja stuff the yaml:
    yaml_text = re.sub("{{.*}}", "replaced", yaml_text)

    # Load the yaml into a dictionary
    return _loadYaml(yaml_text)


def countYamlKeys(yaml_dict):
    """
    Return the number of keys in a dictionary (of dictionaries, of lists, etc.),
    recursively, until the leaf item is no longer a dictionary or list.
    """
    count = 0
    if isinstance(yaml_dict, dict):
        keys = list(yaml_dict.keys())
        count = len(keys)
        for key in keys:
            count += countYamlKeys(yaml_dict[key])
    elif isinstance(yaml_dict, list):
        for item in yaml_dict:
            count += countYamlKeys(item)
    return count


def countSLOC(yaml_files):
    # Remove duplicates in file list:
    unique_yaml_files = list(OrderedDict.fromkeys(yaml_files))

    # Count SLOC:
    sloc = 0
    for yaml_file in unique_yaml_files:
        yaml_data = _openYaml(yaml_file)
        yaml_sloc = countYamlKeys(yaml_data)
        # import sys
        # sys.stderr.write(str(sloc) + " : " + str(yaml_sloc) + " : " + yaml_file + "\n")
        sloc += yaml_sloc
    return sloc


# This python utility is meant to be run from the command
# line. When run it reports the total source lines of code
# found in a list of YAML files.
if __name__ == "__main__":
    class ExtendAction(argparse.Action):
        """
        Arparse extend definition:
        https://stackoverflow.com/questions/41152799/argparse-flatten-the-result-of-action-append
        """
        def __call__(self, parser, namespace, values, option_string=None):
            items = getattr(namespace, self.dest) or []
            items.extend(values)
            setattr(namespace, self.dest, items)

    # Parse the commandline arguments:
    parser = argparse.ArgumentParser(
        description="Given a list of yaml files, report the SLOC (source lines of code)."
    )
    parser.register("action", "extend", ExtendAction)
    parser.add_argument(
        "yaml_files",
        metavar="file.yaml",
        action="extend",
        nargs="+",
        type=str,
        default=[],
        help="A list of yaml files to include in the SLOC calculation.",
    )
    args = parser.parse_args()

    # Calculate SLOC:
    print(countSLOC(args.yaml_files))
