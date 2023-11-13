from base_classes.gprbuild_target_base import gprbuild_target_base

# This file contains a build target base class that should
# work well for most arm bare board tartets.


class arm_bare_board(gprbuild_target_base):
    # ARM bare board targets contain the 32bit and bb (bareboard) path files.
    def path_files(self):
        return super(arm_bare_board, self).path_files() + ["32bit", "bb"]

    # gatmetric info for arm bare board implementations:
    def gnatmetric_info(self, target=""):
        # Return a tuple of:
        # gnatmetric prefix (ie. "arm-eabi-")
        prefix = "arm-eabi-"
        # gnat metric flags
        flags = ""
        return prefix, flags
