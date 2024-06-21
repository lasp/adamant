import os.path
import sys
from util import redo
from util import error
from util import redo_arg
from util import filesystem
from shutil import copyfile
from base_classes.build_rule_base import build_rule_base


class build_publish(build_rule_base):
    """
    This build rule "publishes" PDF documentation. When PDFs
    are built using the build_pdf rule, the are constructed in
    a build/pdf directory. Since these directories are removed
    upon cleaning, they are not permanently saved in the repository.
    In order to permanently save PDF documentation, so that we
    don't always have to recompile them to use them, this "publish"
    rule was created.

    This rule recursively looks from the given directory down for
    .tex file not located in a "build" directory. When it finds one
    it builds the pdf file in the build/pdf directory, and then
    moves it to the same directory as the original .tex file, thus
    "publishing" it into a permanent directory.
    """
    def _build(self, redo_1, redo_2, redo_3):
        pass  # We are overriding build instead since
        # we don't need to usual build boilerplate
        # for publish

    def build(self, redo_1, redo_2, redo_3):
        # Figure out build directory location
        directory = os.path.abspath(os.path.dirname(redo_1))

        # Find all build directories below this directory:
        pdf_files = []
        for root, dirnames, files in filesystem.recurse_through_repo(
            directory, ignore=["build", "generators"]
        ):
            if os.path.basename(root) == "doc":
                tex_files = [f for f in files if f.endswith(".tex")]
                for tex_file in tex_files:
                    pdf_name = redo_arg.get_base_no_ext(tex_file) + ".pdf"
                    pdf_files.append(os.path.join(root, pdf_name))

        if not pdf_files:
            sys.stderr.write("No tex files found in or below '" + directory + "'.\n")
            error.abort(0)

        # Print the publish plan:
        sys.stderr.write("Will be publishing:\n")
        for pdf_file in pdf_files:
            sys.stderr.write(pdf_file + "\n")

        # Make all the pdf files:
        to_redo = []
        pdf_to_copy = []
        for pdf_file in pdf_files:
            pdf_dir = os.path.dirname(pdf_file)
            if os.path.isfile(pdf_dir + os.sep + "publish.do"):
                redo.redo_ifchange(pdf_dir + os.sep + "publish")
            else:
                pdf_name = os.path.basename(pdf_file)
                to_redo.append(
                    os.path.join(pdf_dir, "build" + os.sep + "pdf" + os.sep + pdf_name)
                )
                pdf_to_copy.append(pdf_file)
        redo.redo_ifchange(to_redo)

        # Copy all the pdf files to their final location:
        assert len(to_redo) == len(pdf_to_copy)
        for build_pdf, final_pdf in zip(to_redo, pdf_to_copy):
            copyfile(build_pdf, final_pdf)

    # No need to provide these for "redo publish"
    # def input_file_regex(self): pass
    # def output_filename(self, input_filename): pass
