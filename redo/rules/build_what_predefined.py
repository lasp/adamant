
from base_classes.build_rule_base import build_rule_base
import sys


def get_predefined_targets() -> list:
    """List of `redo` targets that can be run from anywhere in the repository."""
    return [
        "all",
        # "path",
        # "recursive",
        "clean",
        "clean_all",
        "clear_cache",
        "templates",
        "publish",
        "targets",
        "prove",
        "analyze",
        "style",
        "pretty",
        "test_all",
        "coverage_all",
        "style_all",
    ]


class build_what_predefined(build_rule_base):
    """Lists all redo targets which will be available in any subdirectory
    of the project base folder. See `build_what`.
    """

    def build(self, redo_1, redo_2, redo_3):
        """no need to access database"""
        self._build(redo_1, redo_2, redo_3)

    def _build(self, redo_1, redo_2, redo_3):
        redo_targets = sorted(get_predefined_targets())
        sys.stderr.write(
            "redo " + "\nredo ".join(sorted(redo_targets)) + "\n"
        )
