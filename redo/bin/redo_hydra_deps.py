#!/usr/bin/env python3
from xml.etree import ElementTree
import argparse
from util import redo
import os.path


def read_includes(xml_file):
    xml_file_dir = os.path.dirname(xml_file) + os.sep
    root = ElementTree.parse(xml_file).getroot()
    deps = []
    for inc in root.findall("include"):
        if os.path.isabs(inc.text):
            deps.append(inc.text)
        else:
            deps.append(xml_file_dir + inc.text)
    return deps


def depend_on_includes(xml_file):
    deps = read_includes(xml_file)
    redo.redo_ifchange(deps)
    to_return = deps
    for dep in deps:
        to_return += depend_on_includes(dep)
    return to_return


if __name__ == "__main__":
    # Parse the commandline arguments:
    parser = argparse.ArgumentParser(
        description="Run redo-ifchange on all hydra dependencies. " +
                    "You should provide hydra_config.xml to this script."
    )
    parser.add_argument(
        "hydra_config",
        metavar="hydra_config.xml",
        type=str,
        help="The hydra configuration xml file.",
    )
    args = parser.parse_args()

    redo.redo_ifchange(args.hydra_config)
    deps = depend_on_includes(args.hydra_config)
    print(deps)
