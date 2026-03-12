"""
In-process code pre-generation to avoid redo subprocess overhead.

Normally, each generated source file (e.g. autocoded .ads/.adb) is built
by redo spawning a subprocess that invokes build_via_generator. For large
builds this means hundreds of Python subprocess invocations just for
codegen. This module short-circuits that by running generators directly
in the current process, writing output files to disk, and registering
them with redo-done so redo knows they are up-to-date.

Generator instances are cached at module level so that repeated calls
(e.g. across the recursive dependency walk in build_object.py) reuse
the same generator objects and avoid redundant module imports.
"""
import os
import sys
import io

# Cache generator instances across calls to avoid redundant imports.
# Keyed by (module_name, class_name, file_name) -> generator instance.
_generator_cache = {}


def _get_generator_instance(module_name, class_name, file_name):
    """Get or create a cached generator instance."""
    key = (module_name, class_name, file_name)
    if key not in _generator_cache:
        from util import meta
        module = meta.import_module_from_filename(file_name, module_name)
        generator_class = getattr(module, class_name)
        _generator_cache[key] = generator_class()
    return _generator_cache[key]


def pregenerate_codegen_targets(source_files):
    """
    Given source files about to be redo_ifchange'd, identify which ones
    are generator targets, run their generators in-process, write the
    output, and register each with redo-done so redo tracks their
    dependencies for future incremental builds.

    Returns the list of successfully pre-generated targets so the caller
    can exclude them from redo_ifchange (since redo-done already recorded
    their state).
    """
    from database.generator_database import generator_database
    from database.database import DATABASE_MODE
    from util import filesystem
    from util import redo

    if not source_files:
        return []

    pregenerated = []
    try:
        with generator_database(mode=DATABASE_MODE.READ_ONLY) as db:
            for source in source_files:
                # Check if this source is a generator target. Most source
                # files are not generated, so KeyError is the common case.
                try:
                    gen_info = db.get_generator(source)
                except KeyError:
                    continue

                # If the file already exists on disk, don't regenerate it.
                # Instead, let it fall through to redo-ifchange in the caller
                # so redo can check whether it's stale and rebuild if needed.
                # If it is already up to date, the redo-ifchange call will be
                # lightning fast.
                if os.path.isfile(source):
                    continue

                module_name = gen_info[0]
                class_name = gen_info[1]
                file_name = gen_info[2]
                input_filename = gen_info[3]

                # The generator's input model file must exist. If it doesn't,
                # we can't generate in-process — fall through to redo-ifchange
                # which will build the input first.
                if not os.path.isfile(input_filename):
                    continue

                try:
                    # Grab the generator for this file.
                    generator = _get_generator_instance(module_name, class_name, file_name)

                    # Resolve the generator's declared dependencies (model
                    # files, submodels, etc.). If any dependency is missing,
                    # we can't safely generate in-process — fall through to
                    # redo-ifchange which will build the missing deps first.
                    try:
                        dependencies = generator.depends_on(input_filename)
                    except Exception:
                        continue
                    if dependencies:
                        if isinstance(dependencies, str):
                            dependencies = [dependencies]
                        if not all(os.path.isfile(dep) for dep in dependencies):
                            continue

                    filesystem.safe_makedir(os.path.dirname(source))

                    # Generators write to stdout (matching how build_via_generator
                    # works with redo's output capture). Capture stdout and write
                    # the content to the output file ourselves. Stderr is not
                    # captured and thus will be printed to the screen as it
                    # normally would.
                    old_stdout = sys.stdout
                    sys.stdout = captured = io.StringIO()
                    try:
                        generator.generate(input_filename)
                    finally:
                        sys.stdout = old_stdout

                    with open(source, "w") as f:
                        f.write(captured.getvalue())

                    # Register with redo-done so redo records this target as
                    # built with its full dependency set. This mirrors what
                    # build_via_generator.py does implicitly via redo-ifchange
                    # calls inside a .do script. Without this, redo would have
                    # no record of the target and would re-invoke the .do script
                    # on the next build even if nothing changed.
                    #
                    # The deps match build_via_generator: the input model file,
                    # the generator module file, plus generator-specific deps.
                    generator_module_file = sys.modules[generator.__module__].__file__
                    all_deps = [input_filename, generator_module_file]
                    if dependencies:
                        all_deps.extend(dependencies)
                    redo.redo_done(source, all_deps)

                    # Track this file as pre-generated.
                    pregenerated.append(source)
                except Exception:
                    # Generation failed. Clean up any partial output and let
                    # redo handle this target through its normal .do script path.
                    try:
                        if os.path.exists(source):
                            os.remove(source)
                    except Exception:
                        pass
    except Exception:
        # If we can't even open the generator database (e.g. it doesn't
        # exist yet on a fresh build), fall back gracefully — all files
        # will go through the normal redo-ifchange path.
        pass

    return pregenerated


def pregenerate_and_redo_done(source_files):
    """
    Pre-generate source file targets in-process, then redo-ifchange the
    remaining (non-pre-generated) sources. This is the main entry point
    used by build_object.py wherever it would normally call
    redo.redo_ifchange on source files.
    """
    from util import redo

    if not source_files:
        return

    # Deduplicate to avoid redundant generation or redo-ifchange calls.
    source_files = list(dict.fromkeys(source_files))

    # Generate what we can in-process. These targets are registered
    # with redo-done and don't need redo-ifchange.
    pregenerated = set(pregenerate_codegen_targets(source_files))

    # Everything else (non-generator sources, existing files that need
    # staleness checks, generators we couldn't run) goes through
    # redo-ifchange.
    remaining = [s for s in source_files if s not in pregenerated]
    if remaining:
        redo.redo_ifchange(remaining)
