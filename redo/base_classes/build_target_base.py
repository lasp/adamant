import abc


class build_target_base(metaclass=abc.ABCMeta):
    """
    This is the base object for build targets. All other
    build_targets should inherit from this base and implement the
    abstract methods. Build targets specify the compilers, flags,
    libraries, etc. used to compile source code to binaries. You
    might have one target for a Linux development machine, and another
    one that cross compiles to an embedded target.
    """
    # Note: All return types of this class should
    # be strings. Lists, or other objects will not
    # be handled.

    def gnat_stack_limit(self):
        """
        Set the stack limit for the environment task.
        Override this to change.
        See: https://gcc.gnu.org/onlinedocs/gcc-3.4.1/gnat_ugn_unw/Stack-Overflow-Checking.html
        """
        return 10250

    def compiler_prefix(self):
        """
        The default implementation of this is for native, so the string is blank. For
        arm this might be overridden to "arm-eabi-" or something similar. This is prepended
        to ada_compiler, c_compiler etc below:
        """
        return ""

    def instruction_size(self):
        """
        Return either 32 or 64. 32 will be set as default. This controls
        some of the files that get included in the path_files method below, since
        some implementations depend on the bit-size of the processor. Override
        this function if you need something other than 32.
        """
        return 32

    def ada_preprocessor_files(self):
        """
        Return a list of full path preprocessor files to include in the compilation. This
        will usually be a file ending in .adc. Some can be found in
        redo/targets/configuration_pragmas. These will be included in the compilation with
        the -gnatep flag:
        """
        return []

    def ada_configuration_pragma_files(self):
        """
        Return a list of full path configuration pragma files to include in the compilation. This
        will usually be a file ending in .def. Some can be found in
        redo/targets/preprocessor_defs. These will be included in the compilation with the
        -gnatec flag:
        """
        return []

    def rts_path(self):
        """
        This is the full path to the runtime directory. If this is not an empty string then it will
        be included in the compilation with the -RTS flag.
        """
        return ""

    def path_files(self):
        """
        Path files that apply to this target, meaning paths
        found with the path file names returned by this function
        will be included in the build path. A default implementation
        is provided that returns the class name and the bit file (ie. 32bit).
        If "example" is included in the list here than any directories with
        a .example_path file will be added to the build path.
        """
        return [self.__class__.__name__, str(self.instruction_size()) + "bit"]

    @abc.abstractmethod
    def ada_compiler(self):
        """The compiler to be used."""
        pass

    @abc.abstractmethod
    def ada_compiler_flags(self):
        """The compiler flags to be used."""
        pass

    def ada_compiler_extra_includes(self):
        """
        Optional: Return a list of extra include flags
        for the compiler. All includes will be prepended
        by "-I" and separated by a space when put in the
        compilation command.
        Note: All source included in the build_path
        is already automatically included in the compile
        command and does not need to be returned here.
        """
        return []

    def ada_compiler_depends_on(self):
        """
        Optional: Return a list of files to depend on for
        every object compiled with this build target. The default
        implementation is to depend on any preprocessor or
        configuration pragma files found:
        """
        return []

    @abc.abstractmethod
    def ada_binder(self):
        """The binder to be used."""
        pass

    def ada_binder_flags(self):
        """The ada binder flags to be used."""
        return ""  # There usually is no extra flags here.

    def ada_binder_extra_includes(self):
        """
        Optional: Return a string of extra include flags
        for the binder. All includes must be prepended
        by "-I" and separated by a space.
        Note: All source included in the build_path
        is already automatically included in the bind
        command and does not need to be returned here.
        """
        return []

    @abc.abstractmethod
    def ada_linker(self):
        """The linker to be used."""
        pass

    @abc.abstractmethod
    def ada_linker_flags(self):
        """The ada linker flags to be used."""
        pass

    def ada_linker_extra_objects(self):
        """Extra objects to be linked against."""
        return []

    def ada_linker_depends_on(self):
        """
        Optional: Return a list of files to depend on for
        every object compiled with this build target.
        """
        return []

    def gnat_ls(self):
        """
        Optional: Return the executable to run gnat-ls. This
        is used to generate the binary report file.
        """
        return None

    @abc.abstractmethod
    def ada_metrics(self):
        """The compiler to be used."""
        pass

    @abc.abstractmethod
    def c_compiler(self):
        """The C compiler to be used."""
        pass

    @abc.abstractmethod
    def c_compiler_flags(self):
        """The C compiler flags to be used."""
        pass

    def c_compiler_extra_includes(self):
        """
        Optional: Return a list of extra include flags
        for the compiler. All includes will be prepended
        by "-I" and separated by a space when put in the
        compilation command.
        Note: All source included in the build_path
        is already automatically included in the compile
        command and does not need to be returned here.
        """
        return []

    def c_compiler_depends_on(self):
        """
        Optional: Return a list of files to depend on for
        every object compiled with this build target.
        """
        return []

    @abc.abstractmethod
    def cpp_compiler(self):
        """The C++ compiler to be used."""
        pass

    @abc.abstractmethod
    def cpp_compiler_flags(self):
        """The C++ compiler flags to be used."""
        pass

    def cpp_compiler_extra_includes(self):
        """
        Optional: Return a list of extra include flags
        for the compiler. All includes will be prepended
        by "-I" and separated by a space when put in the
        compilation command.
        Note: All source included in the build_path
        is already automatically included in the compile
        command and does not need to be returned here.
        """
        return []

    def cpp_compiler_depends_on(self):
        """
        Optional: Return a list of files to depend on for
        every object compiled with this build target.
        """
        return []


class build_target(object):
    """
    This wrapper class provides the interface used by build_object.py and build_executable.py
    for compilation. It performs some sanitizing and extra work that the above class
    does not provide. It ingests a class of the type above in its init function. You should NOT
    inherit from this class.
    """
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
