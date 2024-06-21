import re
import os.path
from util import redo

# This module contains utility function to assist with generating or
# parsing HTML.


def get_html_links(output_filename, html):
    """

    Given an output filename and the html string that will be
    written to it, search through the html text for html links
    that are included in it.
    ie. <a class="record_link" href="link.html">stuff</a>
    And then return a list of those links
    """
    def relative_path_to_absolute(path_from, path_to):
        return os.path.abspath(os.path.join(path_from, path_to))

    # Search output for html dependencies and depend on them:
    links = list(
        set(
            re.findall(
                r'<a\s+class="record_link"\s+href="(.*)">.*</a>',
                html,
                re.IGNORECASE
            )
        )
    )

    # Get the absolute path to the links from the location of
    # the html output file location:
    path_from = os.path.dirname(output_filename)
    abs_links = [relative_path_to_absolute(path_from, link) for link in links]
    return abs_links


def depend_on_html_links(output_filename, html):
    """
    Given an output filename and the html string that will be
    written to it, search through the html text for html links
    that are included in it.
    ie. <a class="record_link" href="link.html">stuff</a>
    And then depend on those links using redo-ifchange.
    """
    redo.redo_ifchange(get_html_links(output_filename, html))
