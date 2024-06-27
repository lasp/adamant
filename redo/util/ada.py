import re
import os.path
from models.exceptions import ModelException

# This modules provides utility functions related to
# the Ada programming language, such as translating
# a source file name to the Ada package name, formatting
# strings using Ada conventions, and extracting "with"
# dependencies from Ada source files.


def file_name_to_package_name(filename):
    """
    Give an ada source filename, return the
    ada package name.
    """
    basename = os.path.basename(filename)
    return basename.split(".")[0].replace("-", ".")


def is_valid_variable_name(string):
    """Check if a string is a valid ada variable name."""
    try:
        return bool(re.match(r"^[a-zA-Z][_?a-zA-Z0-9]*$", string))
    except TypeError:
        return False


def adaCamelCase(string):
    """
    Return an ada-camel-case-ified string, which would
    turn something like this "some_string_yo.hi" into
    "Some_String_Yo.Hi"
    """
    # Make sure that the
    ss = string.split(".")
    for s in ss:
        if not is_valid_variable_name(s):
            raise ModelException(
                "The string '"
                + string
                + "' is not valid Ada syntax. Ada variables and types must match the regex '^[a-zA-Z][_?a-zA-Z0-9]*$'."
            )

    def upperFirst(x):
        if len(x) > 1:
            return x[0].upper() + x[1:]
        else:
            return x.upper()

    def caseify(string, splitStr):
        return splitStr.join(map(lambda x: upperFirst(x), string.split(splitStr)))

    string = string.lower()
    string = caseify(string, "_")
    string = caseify(string, ".")
    return string.strip()


# uncap = lambda s: s[:1].lower() + s[1:] if s else ""
def formatVariable(string):
    # return uncap(adaCamelCase(string))
    # Now that we are adhering closer to GNAT standard
    # we need to capitalize variables.
    return formatType(string)


def formatType(string):
    return adaCamelCase(string)


def formatPackage(string):
    return formatType(string)


def formatValue(string):
    try:
        if string.lower() == "null":  # Don't reformat null
            return "null"
        elif bool(re.match(r".*\.[iI][rR][qQ][0-9]+$", string)):  # Don't reformat IRQs
            return string
        else:
            return formatVariable(string)
    except Exception:
        return string


def _rawType(adaType):
    """Return the raw ada type without any "()" following it for arrays"""
    return re.sub(r"\([^)]*\)", "", adaType)  # remove ()


def getPackage(adaType):
    """
    Given a string like Command.Awesome.T, return Command.Awesome:
    If there is no package for a type like "A" then just return
    the type.
    """
    return os.path.splitext(_rawType(adaType))[0]


def isTypePrimitive(adaType):
    """
    Is an ada type a primitive type like "Natural" or "Positive"
    Examples of nonprimitive types might be "A.B" or "Command.T"
    """
    return os.path.splitext(_rawType(adaType))[1] == ""


def get_source_dependencies(source_filename):
    """
    Simple function which reads the "with" dependencies from
    an ada program and then returns them in a list.
    """
    from itertools import chain

    def remove_prefix(text, prefix):
        if text.startswith(prefix):
            return text[len(prefix):]
        return text

    def remove_postfix(text, prefix):
        if text.endswith(prefix):
            return text[:-len(prefix)]
        return text

    # Make sure the file is Ada source code:
    assert source_filename.endswith(".ads") or source_filename.endswith(".adb"), (
        "Cannot get dependencies for '"
        + source_filename
        + "' because it is not an Ada source file."
    )

    with open(source_filename, "r") as f:
        content = f.read()

        # Split the file into statements and remove comments:
        statements = re.split("[\n/;]", content)
        statements = [x.strip().split("--")[0] for x in statements]

        # Find all with statements
        r = re.compile(r"^\s*with\s+.*$", re.IGNORECASE)
        with_statements = list(filter(r.match, statements))
        r = re.compile(r"^\s*limited\s+with\s+.*$", re.IGNORECASE)
        with_statements.extend(list(filter(r.match, statements)))
        r = re.compile(r"^\s*private\s+with\s+.*", re.IGNORECASE)
        with_statements.extend(list(filter(r.match, statements)))
        r = re.compile(r"^\s*limited\s+with\s+.*$", re.IGNORECASE)
        with_statements.extend(list(filter(r.match, statements)))
        r = re.compile(r"^\s*private\s+limited\s+with\s+.*$", re.IGNORECASE)
        with_statements.extend(list(filter(r.match, statements)))

        # Filter things out that are not regular with statements:
        # sys.stderr.write(str(with_statements) + "\n") # For debugging
        r = re.compile(r".*is\s+new\s+.*", re.IGNORECASE)
        with_statements = [item for item in with_statements if not r.match(item)]
        r = re.compile(r".*with\s+package\s+.*", re.IGNORECASE)
        with_statements = [item for item in with_statements if not r.match(item)]
        r = re.compile(r".*with\s+function\s+.*", re.IGNORECASE)
        with_statements = [item for item in with_statements if not r.match(item)]
        r = re.compile(r".*with\s+procedure\s+.*", re.IGNORECASE)
        with_statements = [item for item in with_statements if not r.match(item)]
        r = re.compile(r".*with\s+.*=>", re.IGNORECASE)
        with_statements = [item for item in with_statements if not r.match(item)]

        # Remove the "with":
        includes = [
            remove_prefix(x.strip(), "private").strip() for x in with_statements
        ]
        includes = [remove_prefix(x.strip(), "limited").strip() for x in includes]
        includes = [remove_prefix(x.strip(), "with").strip() for x in includes]

        # Account for commas:
        includes = list(chain.from_iterable([x.split(",") for x in includes]))
        includes = [x.strip() for x in includes]

        # If package has a parent package, that is an implicit include:
        r = re.compile(r"package .* is\s*$", re.IGNORECASE)
        packages = list(filter(r.match, statements))
        packages = [remove_prefix(x.strip(), "package").strip() for x in packages]
        packages = [remove_prefix(x.strip(), "body").strip() for x in packages]
        packages = [remove_postfix(x.strip(), "is").strip() for x in packages]
        parents = []
        for package in packages:
            split_package = package.split(".")
            if len(split_package) > 1:
                parents.append(".".join(split_package[:-1]))

        # Print the results:
        includes.extend(parents)
        includes = list(set(includes))
        # Any include that is not a single word is probably in a "generic" statement and we
        # should filter it out:
        includes = list(filter(lambda x: x.split() != 1, includes))
        return includes


def should_depend_on_adb(spec_file):
    """
    Every object depends on any specification file (.ads) of a withed package. Figuring out
    which adb file should be depended on is a bit trickier. The ruled are laid out here:

    https://gcc.gnu.org/onlinedocs/gcc-4.9.2/gnat_ugn/Source-Dependencies.html

    Rules:
    1. If a file being compiled with's a unit X, the object file depends on the file containing the spec of unit X.
    This includes files that are with'ed implicitly either because they are parents of with'ed child units or they
    are run-time units required by the language constructs used in a particular unit.
    2. If a file being compiled instantiates a library level generic unit, the object file depends on both the spec
    and body files for this generic unit.
    3. If a file being compiled instantiates a generic unit defined within a package, the object file depends on the
    body file for the package as well as the spec file.
    4. If a file being compiled contains a call to a subprogram for which pragma Inline applies and inlining is
    activated with the -gnatn switch, the object file depends on the file containing the body of this subprogram
    as well as on the file containing the spec. Note that for inlining to actually occur as a result of the use
    of this switch, it is necessary to compile in optimizing mode.
    5. The use of -gnatN activates inlining optimization that is performed by the front end of the compiler. This
    inlining does not require that the code generation be optimized. Like -gnatn, the use of this switch
    generates additional dependencies.

    When using a gcc-based back end (in practice this means using any version of GNAT other than the JGNAT, .NET
    or GNAAMP versions), then the use of -gnatN is deprecated, and the use of -gnatn is preferred. Historically
    front end inlining was more extensive than the gcc back end inlining, but that is no longer the case.

    If an object file O depends on the proper body of a subunit through inlining or instantiation, it depends on the
    parent unit of the subunit. This means that any modification of the parent unit or one of its subunits affects the
    compilation of O.

    6. The object file for a parent unit depends on all its subunit body files.


    The function below provides a conservative implementation of the rules specified above. Adamant does not have support
    for subunits currently, so that portion is ignored. Provided an ada specification file, this function determines if
    you should also depend on the .adb file by determining if the spec:

    1) Includes any library level generic definitions
    2) Includes any package level generic definitions
    3) Includes any subprograms that are make use of pragma Inline

    These determinations are not quite accurate, but they are conservative. For instance, #1 and #2 really only apply if
    the object that is currently being compiled actually instantiates a generic object. We are not checking if this is
    the case, we just assume it might be since it is within the package. For #3 we do not check to see if the -gnatn
    or -gnatN switch is being used, we just assume it is. This is also conservative.

    Even with this conservative approach, substantial gains in recompilation performance can be seen.
    """
    # Make sure the file is Ada source code:
    assert spec_file.endswith(".ads"), (
        "An Ada specification (.ads) file is required. '"
        + spec_file
        + "' is not a specification file."
    )

    with open(spec_file, "r") as f:
        content = f.read()

        # Split the file into statements and remove comments:
        statements = re.split("[\n/;]", content)
        statements = [x.strip().split("--")[0] for x in statements]

        # See if there is any lines that only contain the word 'generic' on it:
        r = re.compile(r"^\s*generic\s*$", re.IGNORECASE)
        lines_with_only_generic = list(filter(r.match, statements))
        if lines_with_only_generic:
            return True

        # See if there is any lines that specify inlining:
        r = re.compile(r"^.*with\s+inline\s+=>\s+true.*$", re.IGNORECASE)
        lines_with_inline_aspect = list(filter(r.match, statements))
        if lines_with_inline_aspect:
            return True

        r = re.compile(r"^.*pragma\s+inline.*$", re.IGNORECASE)
        lines_with_inline_pragma = list(filter(r.match, statements))
        if lines_with_inline_pragma:
            return True

    return False
