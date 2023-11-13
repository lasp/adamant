from base_classes.gprbuild_target_base import gprbuild_target_base
import os.path

# This file contains the build targets for a generic 64-bit desktop
# Linux system. It contains both a Linux build target as
# well as a Linux_Test build target which includes source
# and linker flags for the AUnit Ada Unit Test library.
# The compilers for both Ada and C/C++ are GNU gcc running
# on top of the GNAT Linux runtime.


class Linux_Base(gprbuild_target_base):
    def path_files(self):
        return list(set(super(Linux_Base, self).path_files() + ["64bit", "Linux"]))


#
# Currently there is no production or development targets for Linux, since
# no flight projects have used the Linux operating system as their target.
#


# This is the standard debug Linux target.
class Linux_Debug(Linux_Base):
    def description(self):
        return ("This native 64-bit Linux target has no optimization, compiles with debug flags"
                "enabled, and enforces the Ravenscar profile.")

    def gpr_project_file(self):
        return os.path.join(
            os.environ["ADAMANT_DIR"],
            "redo" + os.sep + "targets" + os.sep + "gpr" + os.sep + "linux_debug.gpr",
        )


# This is the default Linux target.
class Linux(Linux_Debug):
    def description(self):
        return "The default Linux target. This is simply a rename of Linux_Debug."


# Test target which links in aunit:
class Linux_Test(Linux_Base):
    def description(self):
        return ("Same as Linux_Debug except it does not enforce the Ravenscar profile and links"
                "with AUnit.")

    def gpr_project_file(self):
        return os.path.join(
            os.environ["ADAMANT_DIR"],
            "redo" + os.sep + "targets" + os.sep + "gpr" + os.sep + "linux_test.gpr",
        )


# Converage target which works with gcov:
class Linux_Coverage(Linux_Base):
    def description(self):
        return ("Same as Linux_Test except it adds compilation flags to assist with coverage analysis"
                "via gcov.")

    def path_files(self):
        return list(set(super(Linux_Coverage, self).path_files() + ["Linux_Test"]))

    def gpr_project_file(self):
        return os.path.join(
            os.environ["ADAMANT_DIR"],
            "redo"
            + os.sep
            + "targets"
            + os.sep
            + "gpr"
            + os.sep
            + "linux_coverage.gpr",
        )


# This is the target used with GNATprove to verify SPARK code.
class Linux_Prove(Linux_Debug):
    def description(self):
        return "This target is used to generate the path for calls to GNATprove to analyze SPARK code."
