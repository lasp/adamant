from base_classes.gprbuild_target_base import gprbuild_target_base

# This file contains a build target base class that should
# work well for most arm bare board tartets.


class arm_bare_board(gprbuild_target_base):
    def path_files(self):
        """ARM bare board targets contain the 32bit and bb (bareboard) path files.
        bb_production marks bareboard sources whose semantics differ between production
        and cross unit-test contexts (e.g. Safe_Deallocator). Cross test targets must
        override path_files to drop bb_production and add bb_test."""
        return super(arm_bare_board, self).path_files() + ["32bit", "bb", "bb_production"]

    def gnatmetric_info(self, target=""):
        """gatmetric info for arm bare board implementations."""
        # Return a tuple of:
        # gnatmetric prefix (ie. "arm-eabi-")
        prefix = "arm-eabi-"
        # gnat metric flags
        flags = ""
        return prefix, flags

    def is_cross(self):
        """ARM bareboard ELFs cannot be executed on an x86_64 Linux host."""
        return True
