from database.redo_target_database import redo_target_database
import os.path
from util import redo
from util import filesystem
from util import shell
from util import model_loader
from base_classes.build_rule_base import build_rule_base
import sys
import re
import glob


# This command is the equivalent of turning on style checking
# and recompiling all objects within the current directory. ie.
#
#  $ export CHECK_STYLE=True
#  $ redo object_1.o object_2.o etc.
#
# Using "redo style" is quicker and more convenient for checking
# that source code meets the style requirements than using the
# environment variable directly.
class build_style(build_rule_base):
    def _build(self, redo_1, redo_2, redo_3):
        # Get targets for this directory:
        directory = os.path.abspath(os.path.dirname(redo_1))
        build_directory = os.path.join(directory, "build")
        style_directory = os.path.join(build_directory, "style")
        py_directory = os.path.join(build_directory, "py")
        resolved_yaml_directory = os.path.join(style_directory, "resolved_yaml")
        targets = []
        with redo_target_database() as db:
            try:
                targets = db.get_targets_for_directory(directory)
            except KeyError:
                pass

        # Find all the objects that can be built in in this build directory, minus any assertion objects since
        # those are not cross platform, and minus .C packages, since those don't compile for every packed type.
        assertion_obj_reg = re.compile(r".*build/obj/.*\-assertion.o$")
        c_obj_reg = re.compile(r".*build/obj/.*\-c.o$")
        objects = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and target.endswith(".o")
            and not assertion_obj_reg.match(target)
            and not c_obj_reg.match(target)
        ]

        # Turn on the style checking:
        os.environ["CHECK_STYLE"] = "True"

        # Simply force recompile all the objects in this directory with style checking enabled
        # and we will get nice style check messages.
        filesystem.safe_makedir(style_directory)
        style_log_file = style_directory + os.sep + "style.log"
        style_log_file_temp = style_directory + os.sep + "style.log.temp"
        redo.redo(
            objects
            + ["2>&1", "|",  "tee", style_log_file_temp, ">&2"]
        )

        # Filter output log to only include warnings:
        if os.path.isfile(style_log_file_temp):
            shell.run_command("cat " + style_log_file_temp + r" | grep '^.*\.ad[b,s]:[0-9]*' > " + style_log_file + " | true")
            os.remove(style_log_file_temp)

        # Now we need to check any python that we find:
        objects = [
            target
            for target in targets
            if os.path.dirname(target).startswith(build_directory)
            and (target.endswith(".py") or target.endswith(".yaml"))
        ]
        redo.redo(objects)
        flake_cmd_prefix = "flake8 --max-line-length=140 --ignore=E121,E123,E126,E226,E24,E704,W503,W504,E402 "
        flake_cmd_suffix = " 2>&1 | tee -a " + style_log_file + " 1>&2"
        flake_target = ""
        if glob.glob(directory + os.sep + "*.py"):
            flake_target = "*.py"
        if os.path.isdir(py_directory) and glob.glob(py_directory + os.sep + "*.py"):
            flake_target += " build" + os.sep + "py" + os.sep + "*.py"
        if flake_target:
            flake_cmd = flake_cmd_prefix + flake_target + flake_cmd_suffix
            cwd = os.getcwd()
            os.chdir(directory)
            shell.run_command(flake_cmd)
            os.chdir(cwd)

        # Run codespell check:
        codespell_cwd = directory + os.sep + "*.*"
        codespell_ignore = os.environ["ADAMANT_DIR"] + os.sep + "redo" + os.sep + "codespell" + os.sep + "ignore_list.txt"
        codespell_output = "\" 2>&1 | tee -a " + style_log_file + " 1>&2"
        codespell_build_dir = "*" + os.sep + "build" + os.sep + "obj,*" + os.sep + "build" + os.sep + "bin"
        codespell_skip = " --skip=\"*" + os.sep + "alire," + codespell_build_dir + ",*.pdf,*.eps,*.svg," + style_directory
        codespell_suffix = " -I " + codespell_ignore + codespell_skip + codespell_output
        shell.run_command("codespell "
                          + codespell_cwd
                          + codespell_suffix)
        shell.run_command("codespell "
                          + build_directory
                          + codespell_suffix)

        # Finally lint any YAML files:
        def _yaml_lint(filenames=[]):
            cmd = (
                "yamllint -f parsable -c "
                + os.environ["SCHEMAPATH"]
                + "/yaml_lint_config.yaml "
                + " ".join(filenames)
                + flake_cmd_suffix
            )
            shell.run_command(cmd)

        yaml_files = glob.glob(directory + os.sep + "*.yaml")
        yaml_files.extend(glob.glob(directory + os.sep + "build" + os.sep + "yaml" + os.sep + "*.yaml"))
        if yaml_files:
            filesystem.safe_makedir(resolved_yaml_directory)
            # Yaml files may have unresolved Jinja2 templating to be filled in by the config
            # file. To lint these properly we need to resolve them first. Store resolved yamls
            # in build/style/resolved_yaml and then run linter on those:
            self.config = model_loader.load_project_configuration()
            for yaml in yaml_files:
                if not yaml.endswith("configuration.yaml"):
                    resolved_yaml = self.config.render(yaml, template_path=os.sep)
                    with open(os.path.join(resolved_yaml_directory, os.path.basename(yaml)), "w") as f:
                        f.write(resolved_yaml)

            cwd = os.getcwd()
            os.chdir(resolved_yaml_directory)
            resolved_yaml_files = glob.glob("*.yaml")
            if resolved_yaml_files:
                _yaml_lint(resolved_yaml_files)
            os.chdir(cwd)

        sys.stderr.write(
            "Any style messages that appear above have also been written to: "
            + style_log_file
            + "\n"
        )

    # No need to provide these for "redo style"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
