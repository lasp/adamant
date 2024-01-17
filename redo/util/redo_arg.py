import os.path

# This modules provides utility functions for manipulating
# filenames, particularly the filenames that are provided
# as arguments to .do files for redo, ie. $1 $2 and $3.


# Given a filename return the directory, basename,
# and extension as a tuple. ie.
# /path/to/file.txt -> (/path/to, file, .txt)
def split_full_filename(full_filename):
    dirname = os.path.dirname(full_filename)
    filename = os.path.basename(full_filename)
    basename, ext = os.path.splitext(filename)
    return dirname, basename, ext


# Extracts useful info from a model filename. Examples:
#
# /path/to/my_component.component.yaml -> (/path/to, None, my_component, component, yaml)
# /path/to/my_test.my_component.tests.yaml -> (/path/to, my_test, my_component, tests, yaml)
#
def split_model_filename(input_filename, expected_model_type=None):
    # Get the current directory and basename of the input filename:
    dirname, basename, ext = split_full_filename(input_filename)
    assert ext == ".yaml"
    name, model_type = os.path.splitext(basename)
    if expected_model_type:
        assert model_type == "." + expected_model_type
    a = name.split(".")
    specific_name = None
    if len(a) == 1:
        model_name = a[0]
    else:
        model_name = a[1]
        specific_name = a[0]
    return dirname, specific_name, model_name, model_type[1:], ext[1:]


# Given a filename return the directory and
# filename, ie.
# /path/to/file.txt -> (/path/to, file.txt)
def split_redo_arg(redo_arg):
    base = os.path.basename(redo_arg)
    directory = os.path.dirname(redo_arg)
    return directory, base


# Helper functions which return directories
# found in a file path.
def _get_1_dir_path(path):
    dir_1 = os.path.dirname(path)
    dir_1_name = os.path.basename(dir_1)
    return dir_1, dir_1_name


def _get_1_dir(path):
    return _get_1_dir_path(path)[1]


def _get_2_dirs(path):
    dir_1 = os.path.dirname(path)
    dir_1_name = os.path.basename(dir_1)
    dir_2 = os.path.dirname(dir_1)
    dir_2_name = os.path.basename(dir_2)
    return dir_2_name, dir_1_name


def _get_3_dirs(path):
    dir_1 = os.path.dirname(path)
    dir_1_name = os.path.basename(dir_1)
    dir_2 = os.path.dirname(dir_1)
    dir_2_name = os.path.basename(dir_2)
    dir_3 = os.path.dirname(dir_2)
    dir_3_name = os.path.basename(dir_3)
    return dir_3_name, dir_2_name, dir_1_name


# Given a filename check if the file is found
# in a directory like /path/to/build/obj/Linux
def in_build_obj_dir(redo_arg):
    build_dir, obj_dir, target_dir = _get_3_dirs(redo_arg)
    return build_dir == "build" and obj_dir == "obj" and target_dir


# Given a filename check if the file is found
# in a directory like /path/to/build/bin/Linux
def in_build_bin_dir(redo_arg):
    build_dir, obj_dir, target_dir = _get_3_dirs(redo_arg)
    return build_dir == "build" and obj_dir == "bin" and target_dir


# Given a filename check if the file is found
# in a directory like /path/to/build/src
def in_build_src_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "src"


# Given a filename check if the file is found
# in a directory like /path/to/build/yaml
def in_build_yaml_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "yaml"


# Given a filename check if the file is found
# in a directory like /path/to/build/tex
def in_build_tex_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "tex"


# Given a filename check if the file is found
# in a directory like /path/to/build/svg
def in_build_svg_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "svg"


# Given a filename check if the file is found
# in a directory like /path/to/build/eps
def in_build_eps_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "eps"


# Given a filename check if the file is found
# in a directory like /path/to/build/png
def in_build_png_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "png"


# Given a filename check if the file is found
# in a directory like /path/to/build/pdf
def in_build_pdf_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "pdf"


# Given a filename check if the file is found
# in a directory like /path/to/build/gpr
def in_build_gpr_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "gpr"


# Given a filename check if the file is found
# in a directory like /path/to/build/template
def in_build_template_dir(redo_arg):
    build_dir, src_dir = _get_2_dirs(redo_arg)
    return build_dir == "build" and src_dir == "template"


# Given a filename check if the file is found
# in a directory like /path/to/build/metric
def in_build_metric_dir(redo_arg):
    build_dir, obj_dir, target_dir = _get_3_dirs(redo_arg)
    return build_dir == "build" and obj_dir == "metric" and target_dir


# Given a filename get the root source directory
# for it. ie.
# /path/to/build/obj/Linux/file.o -> /path/to
def get_build_dir(redo_arg):
    dir_path, dir_name = _get_1_dir_path(redo_arg)
    while dir_name != "":
        dir_path, dir_name = _get_1_dir_path(dir_path)
        if dir_name == "build":
            return dir_path
    raise Exception(redo_arg + " not in build directory.")


# Given a filename get the root source directory
# for it. ie.
# /path/to/build/obj/Linux/file.o -> /path/to
def get_src_dir(redo_arg):
    dir_path, dir_name = _get_1_dir_path(redo_arg)
    to_return = dir_path
    while dir_name != "":
        dir_path, dir_name = _get_1_dir_path(dir_path)
        if dir_name == "build":
            to_return = os.path.dirname(dir_path)
            break
    if to_return:
        return to_return
    else:
        return "."


# Given a filename check if the file is found
# in a build directory, or a subdirectory of
# a build directory. ie.
# /path/to/build/file.txt -> True
# /path/to/build/obj/Linux/file.o -> True
# /path/to/file.svg -> False
def in_build_dir(redo_arg):
    dir_path, dir_name = _get_1_dir_path(redo_arg)
    while dir_name != "":
        if dir_name == "build":
            return True
        dir_path, dir_name = _get_1_dir_path(dir_path)
    return False


# Given a filename get the build target for that
# file, if it exists. Otherwise, assert. ie.
# /path/to/build/obj/Linux/file.o -> Linux
def get_target(redo_arg):
    assert (
        in_build_obj_dir(redo_arg)
        or in_build_bin_dir(redo_arg)
        or in_build_metric_dir(redo_arg)
    )
    return _get_1_dir(redo_arg)


# Given a filename get the base filename with
# no extension of directory, ie.
# /path/to/file.txt -> file
def get_base_no_ext(source_filename):
    basename = os.path.basename(source_filename)
    base, ext = os.path.splitext(basename)
    return base


# Given a source filename and the current build target
# compute the associated object filename. ie.
# /path/to/file.adb -> /path/to/build/obj/Linux/file.o
def src_file_to_obj_file(source_filename, target):
    src_dir = get_src_dir(source_filename)
    src_base = get_base_no_ext(source_filename)
    return os.path.join(
        src_dir, "build" + os.sep + "obj" + os.sep + target + os.sep + src_base + ".o"
    )
