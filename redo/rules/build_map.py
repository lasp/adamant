import os.path
from util import redo
from util import error
from util import redo_arg
from base_classes.build_rule_base import build_rule_base


class build_map(build_rule_base):
    """
    This build rule turns an executable ".elf" file into a ".map.json"
    address map for a viewer: the component instance records with their
    field layouts, the connector handler entry addresses and the ELF's
    digest, read from the DWARF and symbol table. It needs pyelftools.
    """
    def _build(self, redo_1, redo_2, redo_3):
        if not redo_arg.in_build_bin_dir(redo_1):
            error.error_abort(
                "Map file '"
                + redo_1
                + "' can only be built in a 'build/bin/<target>' directory."
            )

        # Depend on the elf file associated with this map. It has to
        # reside in the same build/bin/<target> directory.
        bin_dir, base_name = redo_arg.split_redo_arg(redo_2)
        base_name = base_name[: -len(".map")] if base_name.endswith(".map") else base_name
        elf_file = os.path.join(bin_dir, base_name + ".elf")
        redo.redo_ifchange(elf_file)

        try:
            from util import address_map
            import elftools  # noqa: F401
        except ImportError:
            error.error_abort(
                "Building '" + redo_1 + "' needs the pyelftools Python package."
            )

        address_map.write_map(elf_file, redo_3)

    def input_file_regex(self):
        """Matches any ".elf" file."""
        return r".*\.elf$"

    def output_filename(self, input_filename):
        """
        Produces a map file with the same name as the ".elf"
        file, in the same build/bin/<target> directory.
        """
        base = redo_arg.get_base_no_ext(input_filename)
        directory = os.path.dirname(input_filename)
        return os.path.join(directory, base + ".map.json")
