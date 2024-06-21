import os.path
from util import error
from util import redo_arg
from util import filesystem
from util import jinja
from base_classes.build_rule_base import build_rule_base
from database.utility_database import utility_database
from util import model_loader


class build_gpr(build_rule_base):
    """
    This build rule generates a gpr file from any executable found. It
    uses the generator system to do the file generation.
    """
    def _build(self, redo_1, redo_2, redo_3):
        if not redo_arg.in_build_gpr_dir(redo_1):
            error.error_abort(
                "GPR file '"
                + redo_1
                + "' can only be built in a 'build/gpr' directory."
            )

        # Make the gpr directory:
        src_dir = redo_arg.get_src_dir(redo_2)
        basename = redo_arg.get_base_no_ext(redo_2)
        gpr_dir = os.path.join(src_dir, "build" + os.sep + "gpr")
        obj_dir = os.path.join(gpr_dir, "obj")
        filesystem.safe_makedir(gpr_dir)
        filesystem.safe_makedir(obj_dir)

        # Use the gpr generator to produce this file. Output
        # to stdout.
        gpr_dict = dict()
        gpr_dict["name"] = redo_arg.get_base_no_ext(redo_1)
        gpr_dict["root"] = "./"
        gpr_dict["obj_dir"] = "./obj"
        gpr_dict["main"] = os.path.relpath(
            os.path.join(src_dir, basename + ".adb"), gpr_dir
        )
        gpr_dict["config"] = model_loader.load_project_configuration()
        with utility_database() as db:
            path = []
            for directory in db.get_source_build_path():
                path.append(os.path.relpath(directory, gpr_dir))
            gpr_dict["source_path"] = path
        print(jinja.render(gpr_dict, "/gpr/project.gpr"))

    def input_file_regex(self):
        """Match any executable file."""
        return [r".*\/main\.elf$", r".*\/test\.elf$"]

    def output_filename(self, input_filename):
        """
        Return a gpr file with the same name as the executable located in the
        build/gpr directory in the same build directory as the executable.
        """
        src_dir = redo_arg.get_src_dir(input_filename)
        base = redo_arg.get_base_no_ext(input_filename)
        return os.path.join(src_dir, "build" + os.sep + "gpr" + os.sep + base + ".gpr")
