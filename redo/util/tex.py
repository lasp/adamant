import re
import os.path
from util import redo
from util import redo_arg

# This module contains utility function to assist with generating or
# parsing tex.


def _depend_on_tex_links(tex, base_dir=None):
    r"""
    Given a string of tex, find any tex links, ie.
    \input{stuff.tex} and then redo_ifchange on those
    files.
    """
    # Ignore comments:
    tex = re.sub(r"%.*\n", "\n", tex)
    # Search output for tex dependencies and depend on them:
    links = list(set(re.findall(r"\\input{(.*)}", tex, re.IGNORECASE)))
    # Resolve paths:
    if base_dir:
        links = [link if os.path.isabs(link) else os.path.join(base_dir, link) for link in links]
    # Depend on the links (assuming they are absolute paths):
    links_to_build = [link for link in links if redo_arg.in_build_tex_dir(link)]
    if links_to_build:
        redo.redo_ifchange(links_to_build)
    return links


def _depend_on_tex_graphics(tex, base_dir=None):
    r"""
    Given a string of tex, find any graphics links, ie.
    \includegraphics{stuff.tex} and then redo_ifchange on those
    files.
    """
    # Ignore comments:
    tex = re.sub(r"%.*\n", "\n", tex)
    # Search output for tex dependencies and depend on them:
    links = list(set(re.findall(r"\\includegraphics.*{(.*)}", tex, re.IGNORECASE)))
    # Resolve paths:
    if base_dir:
        links = [link if os.path.isabs(link) else os.path.join(base_dir, link) for link in links]
    # Depend on the links (assuming they are absolute paths):
    redo.redo_ifchange(links)
    return links


def _depend_on_tex_lstinputlistings(tex, base_dir=None):
    r"""
    Given a string of tex, find any lstinputlisting tags, ie.
    \listinputlisting{stuff.yaml} and then redo_ifchange on those
    files.
    """
    # Ignore comments:
    tex = re.sub(r"%.*\n", "\n", tex)
    # Search output for tex dependencies and depend on them:
    links = list(set(re.findall(r"\\lstinputlisting.*{(.*)}", tex, re.IGNORECASE)))
    links += list(set(re.findall(r"\\yamlcodef.*{(.*)}", tex, re.IGNORECASE)))
    links += list(set(re.findall(r"\\adacodef.*{(.*)}", tex, re.IGNORECASE)))
    links += list(set(re.findall(r"\\pythoncodef.*{(.*)}", tex, re.IGNORECASE)))
    # Resolve paths:
    if base_dir:
        links = [link if os.path.isabs(link) else os.path.join(base_dir, link) for link in links]
    # Depend on the links (assuming they are absolute paths):
    redo.redo_ifchange(links)
    return links


def depend_on_tex_file(tex_file, base_dir):
    """Recursive function that does proper dependency building of tex files."""
    # Read the contents of the tex file:
    assert tex_file.endswith(
        ".tex"
    ), "Unexpected file type found as dependency in: " + str(tex_file)
    try:
        with open(tex_file, "r") as f:
            the_tex = f.read()
    except FileNotFoundError:
        the_tex = None

    if the_tex:
        # Build any graphics in this file:
        _depend_on_tex_graphics(the_tex, base_dir)

        # Build any lstinputlistings in this file:
        _depend_on_tex_lstinputlistings(the_tex, base_dir)

        # Build our dependencies:
        dependencies = _depend_on_tex_links(the_tex, base_dir)

        # Let's build our dependencies' dependencies:
        if dependencies:
            for dep in dependencies:
                depend_on_tex_file(dep, os.path.dirname(dep))
