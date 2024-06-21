import abc
import os
from shutil import which


class gprbuild_target_base(metaclass=abc.ABCMeta):
    def __init__(self):
        # We need to disable prebuilding for any coverage mode target.
        # Since prebuilding builds objects in a temp directory, coverage
        # files are also built in the temp directory. Instead of manually
        # copying these over, we can just do coverage without the prebuilding
        # feature. It will run slower, but we do not run coverage reports often,
        # so this is not a huge deal.
        #
        # We automatically detect a coverage target by looking at its class name.

        if self.__class__.__name__.lower().endswith("coverage"):
            import os

            os.environ["DISABLE_PREBUILD"] = "TRUE"

    @property
    def name(self):
        return self.__class__.__name__

    def description(self):
        """This should be overridden by inheriting classes."""
        return "No description provided."

    @abc.abstractmethod
    def gpr_project_file(self):
        """Full path to the project (.gpr) file to use."""
        pass

    def path_files(self):
        """
        Path files that apply to this target, meaning paths
        found with the path file names returned by this function
        will be included in the build path. A default implementation
        is provided that returns the the class name of this target and
        all targets it inherits from, except the gprbuild_target_base
        and the bit file (ie. 32bit).
        If "example" is included in the list here than any directories with
        a .example_path file will be added to the build path.
        """
        return [c.__name__ for c in self.__class__.__mro__[0:-2]]

    def gprbuild_flags(self):
        """Additional flags to pass to gprbuild should be listed here."""
        return None

    def _get_gnat_install_dir(self):
        """Helper to get gnat install directory for the current target."""
        gnatls_ret = which("gnatls")
        if gnatls_ret:
            return os.path.dirname(os.path.dirname(gnatls_ret))
        else:
            from util import error

            error.error_abort(
                "Error: Could not find GNAT install directory for TARGET="
                + self.__class__.__name__
                + ". Have you sourced setenv.sh?"
            )

    def gnatmetric_info(self, target=""):
        """
        So gnatmetric does not work well with project files, at least in the
        unique way that Adamant uses .gpr files. The build system instead calls
        gnatmetric without any reference to project files. This is unfortunate
        because we are requiring the user to redefine some of the information
        flags/etc here in order to get gnatmetric to work. This is a hack,
        but is a solution that works for now. Building metrics is not
        used as commonly or is as essential as compilation, so a bit of oddness
        here is acceptable. Defining this method for a new build target is awkward,
        but hopefully it is the only such method like this.
        """
        # Return a tuple of:
        # gnatmetric prefix (ie. "arm-eabi-")
        prefix = ""
        # gnat metric flags (for bare-board builds this is often blank)
        flags = ""
        if target.endswith("Test"):
            flags += " -I" + os.path.join(
                self._get_gnat_install_dir(), "include" + os.sep + "aunit"
            )
        return prefix, flags
