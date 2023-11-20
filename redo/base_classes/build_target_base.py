import abc


# This is the base object for build targets. All other
# build_targets should inherit from this base and implement the
# abstract methods. Build targets specify the compilers, flags,
# libraries, etc. used to compile source code to binaries. You
# might have one target for a Linux development machine, and another
# one that cross compiles to an embedded target.
class build_target_base(metaclass=abc.ABCMeta):
    # Note: All return types of this class should
    # be strings. Lists, or other objects will not
    # be handled.

    # Set the stack limit for the environment task.
    # Override this to change.
    # See: https://gcc.gnu.org/onlinedocs/gcc-3.4.1/gnat_ugn_unw/Stack-Overflow-Checking.html
    def gnat_stack_limit(self):
        return 10250

    # The default implementation of this is for native, so the string is blank. For
    # arm this might be overridden to "arm-eabi-" or something similiar. This is preprended
    # to ada_compiler, c_compiler etc below:
    def compiler_prefix(self):
        return ""

    # Return either 32 or 64. 32 will be set as default. This controls
    # some of the files that get included in the path_files method below, since
    # some implementations depend on the bit-size of the processor. Override
    # this function if you need something other than 32.
    def instruction_size(self):
        return 32

    # Return a list of full path preprocessor files to include in the compilation. This
    # will usually be a file ending in .adc. Some can be found in
    # redo/targets/configuration_pragmas. These will be included in the compilation with
    # the -gnatep flag:
    def ada_preprocessor_files(self):
        return []

    # Return a list of full path configuration pragma files to include in the compilation. This
    # will usually be a file ending in .def. Some can be found in
    # redo/targets/preprocessor_defs. These will be included in the compilation with the
    # -gnatec flag:
    def ada_configuration_pragma_files(self):
        return []

    # This is the full path to the runtime directory. If this is not an empty string then it will
    # be included in the compilation with the -RTS flag.
    def rts_path(self):
        return ""

    # Path files that apply to this target, meaning paths
    # found with the path file names returned by this function
    # will be included in the build path. A default implementation
    # is provided that returns the class name and the bit file (ie. 32bit).
    # If "example" is included in the list here than any directories with
    # a .example_path file will be added to the build path.
    def path_files(self):
        return [self.__class__.__name__, str(self.instruction_size()) + "bit"]

    # The compiler to be used.
    @abc.abstractmethod
    def ada_compiler(self):
        pass

    # The compiler flags to be used.
    @abc.abstractmethod
    def ada_compiler_flags(self):
        pass

    # Optional: Return a list of extra include flags
    # for the compiler. All includes will be prepended
    # by "-I" and seperated by a space when put in the
    # compilation command.
    # Note: All source included in the build_path
    # is already automatically included in the compile
    # command and does not need to be returned here.
    def ada_compiler_extra_includes(self):
        return []

    # Optional: Return a list of files to depend on for
    # every object compiled with this build target. The default
    # implementation is to depend on any preprocessor or
    # configuration pragma files found:
    def ada_compiler_depends_on(self):
        return []

    # The binder to be used.
    @abc.abstractmethod
    def ada_binder(self):
        pass

    # The ada binder flags to be used.
    def ada_binder_flags(self):
        return ""  # There usually is no extra flags here.

    # Optional: Return a string of extra include flags
    # for the binder. All includes must be prepended
    # by "-I" and seperated by a space.
    # Note: All source included in the build_path
    # is already automatically included in the bind
    # command and does not need to be returned here.
    def ada_binder_extra_includes(self):
        return []

    # The linker to be used.
    @abc.abstractmethod
    def ada_linker(self):
        pass

    # The ada linker flags to be used.
    @abc.abstractmethod
    def ada_linker_flags(self):
        pass

    # Extra objects to be linked against.
    def ada_linker_extra_objects(self):
        return []

    # Optional: Return a list of files to depend on for
    # every object compiled with this build target.
    def ada_linker_depends_on(self):
        return []

    # Optional: Return the executable to run gnat-ls. This
    # is used to generate the binary report file.
    def gnat_ls(self):
        return None

    # The compiler to be used.
    @abc.abstractmethod
    def ada_metrics(self):
        pass

    # The C compiler to be used.
    @abc.abstractmethod
    def c_compiler(self):
        pass

    # The C compiler flags to be used.
    @abc.abstractmethod
    def c_compiler_flags(self):
        pass

    # Optional: Return a list of extra include flags
    # for the compiler. All includes will be prepended
    # by "-I" and seperated by a space when put in the
    # compilation command.
    # Note: All source included in the build_path
    # is already automatically included in the compile
    # command and does not need to be returned here.
    def c_compiler_extra_includes(self):
        return []

    # Optional: Return a list of files to depend on for
    # every object compiled with this build target.
    def c_compiler_depends_on(self):
        return []

    # The C++ compiler to be used.
    @abc.abstractmethod
    def cpp_compiler(self):
        pass

    # The C++ compiler flags to be used.
    @abc.abstractmethod
    def cpp_compiler_flags(self):
        pass

    # Optional: Return a list of extra include flags
    # for the compiler. All includes will be prepended
    # by "-I" and seperated by a space when put in the
    # compilation command.
    # Note: All source included in the build_path
    # is already automatically included in the compile
    # command and does not need to be returned here.
    def cpp_compiler_extra_includes(self):
        return []

    # Optional: Return a list of files to depend on for
    # every object compiled with this build target.
    def cpp_compiler_depends_on(self):
        return []


# This wrapper class provides the interface used by build_object.py and build_executable.py
# for compilation. It performs some sanitizing and extra work that the above class
# does not provide. It ingests a class of the type above in its init function. You should NOT
# inherit from this class.
class build_target(object):
    def __init__(self, build_target_base_obj):
        self.btb = build_target_base_obj

    # Helpers:
    def __ada_compiler_constructor(self, compiler_root):
        return (
            (
                ("GNAT_STACK_LIMIT=" + str(self.btb.gnat_stack_limit()) + " ")
                if self.btb.gnat_stack_limit()
                else ""
            )
            + self.btb.compiler_prefix()
            + compiler_root
        ).strip()

    def __compiler_constructor(self, compiler_root):
        return (self.btb.compiler_prefix() + compiler_root).strip()

    def __flag_constructor(self, flag_root):
        rts_dir = self.btb.rts_path().strip()
        return (("--RTS=" + rts_dir if rts_dir else "") + " " + flag_root).strip()

    # Methods for build_object.py:
    def ada_compiler(self):
        return self.__ada_compiler_constructor(self.btb.ada_compiler())

    def c_compiler(self):
        return self.__compiler_constructor(self.btb.c_compiler())

    def cpp_compiler(self):
        return self.__compiler_constructor(self.btb.cpp_compiler())

    def ada_linker(self):
        return self.__ada_compiler_constructor(self.btb.ada_linker())

    def ada_binder(self):
        return self.__ada_compiler_constructor(self.btb.ada_binder())

    def ada_metrics(self):
        return self.__ada_compiler_constructor(self.btb.ada_metrics())

    def gnat_ls(self):
        return self.__compiler_constructor(self.btb.gnat_ls())

    def ada_preprocessor_files(self):
        return [inc.strip() for inc in self.btb.ada_preprocessor_files()]

    def ada_configuration_pragma_files(self):
        return [inc.strip() for inc in self.btb.ada_configuration_pragma_files()]

    def ada_compiler_depends_on(self):
        return list(
            set(
                self.btb.ada_compiler_depends_on()
                + self.ada_preprocessor_files()
                + self.ada_configuration_pragma_files()
            )
        )

    def ada_linker_depends_on(self):
        return list(set(self.btb.ada_linker_depends_on()))

    def c_compiler_depends_on(self):
        return list(set(self.btb.c_compiler_depends_on()))

    def cpp_compiler_depends_on(self):
        return list(set(self.btb.cpp_compiler_depends_on()))

    def ada_compiler_flags(self):
        flags = self.__flag_constructor(self.btb.ada_compiler_flags())
        preprocessor_files = self.ada_preprocessor_files()
        if preprocessor_files:
            flags += " -gnatep=" + " -gnatep=".join(preprocessor_files)
        config_pragma_files = self.ada_configuration_pragma_files()
        if config_pragma_files:
            flags += " -gnatec=" + " -gnatec=".join(config_pragma_files)
        return flags

    def c_compiler_flags(self):
        return self.btb.c_compiler_flags().strip()

    def cpp_compiler_flags(self):
        return self.btb.cpp_compiler_flags().strip()

    def ada_binder_flags(self):
        return self.__flag_constructor(self.btb.ada_binder_flags())

    def ada_linker_flags(self):
        return self.__flag_constructor(self.btb.ada_linker_flags())

    def ada_compiler_extra_includes(self):
        includes = [inc.strip() for inc in self.btb.ada_compiler_extra_includes()]
        preprocessor_files = self.ada_preprocessor_files()
        if preprocessor_files:
            includes.extend(preprocessor_files)
        return includes

    def ada_binder_extra_includes(self):
        return [inc.strip() for inc in self.btb.ada_binder_extra_includes()]

    def c_compiler_extra_includes(self):
        return [inc.strip() for inc in self.btb.c_compiler_extra_includes()]

    def cpp_compiler_extra_includes(self):
        return [inc.strip() for inc in self.btb.cpp_compiler_extra_includes()]

    def ada_linker_extra_objects(self):
        return [inc.strip() for inc in self.btb.ada_linker_extra_objects()]
