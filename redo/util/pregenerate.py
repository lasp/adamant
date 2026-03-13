"""
Parallel in-process code pre-generation.

Normally each generated source file is built by redo spawning a subprocess
that runs build_via_generator.  For large builds this means hundreds of
Python subprocess invocations just for codegen.  This module short-circuits
that by running generators directly in a multiprocessing pool, writing
output files to disk, and registering them with redo-done so redo knows
they are up-to-date.

Generation is parallelized across all available CPU cores.  Each worker
process creates its own generator instances and in-memory model cache.
The shared on-disk model_cache.db supports concurrent readers (READ_ONLY
requires no lock) and serializes the occasional cache-miss write via file
locks, so no additional synchronization is needed here.

After all workers finish, the main process registers every successfully
generated file with redo-done in serial.  redo-done is a subprocess call
that updates redo's own database, and we have not verified that redo's
database handles concurrent writes, so we keep this step single-threaded.
"""
import io
import multiprocessing
import os
import sys


# Worker - runs a child process via multiprocessing.Pool
def _generate_one(work_item):
    """
    Generate a single source file in a worker process.

    Returns (source_path, dep_list) on success, or None on failure.
    Failures are logged to stderr and the caller falls back to the
    normal redo-ifchange path for that file.
    """
    source, module_name, class_name, file_name, input_filename = work_item

    try:
        from util import meta
        from util import filesystem

        # Each worker creates its own generator instance. The generator's
        # model_object() method caches the deserialized model in-memory, so
        # the second call (generate after depends_on) is essentially free.
        module = meta.import_module_from_filename(file_name, module_name)
        generator = getattr(module, class_name)()

        filesystem.safe_makedir(os.path.dirname(source))

        # Resolve dependencies first, matching build_via_generator.py order.
        try:
            dependencies = generator.depends_on(input_filename)
        except Exception:
            dependencies = None
        if dependencies and isinstance(dependencies, str):
            dependencies = [dependencies]

        # Generators write to stdout (redo convention). Capture the output
        # and write it to the target file ourselves.
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()
        try:
            generator.generate(input_filename)
        finally:
            sys.stdout = old_stdout

        with open(source, "w") as f:
            f.write(captured.getvalue())

        # Build the full dep list for redo-done including input model file,
        # generator's Python module, plus any generator-declared deps.
        generator_module_file = sys.modules[generator.__module__].__file__
        all_deps = [input_filename, generator_module_file]
        if dependencies:
            all_deps.extend(dependencies)

        return (source, all_deps)

    except Exception:
        # Generation failed. Clean up any partial output and let
        # redo handle this target through its normal .do script path.
        try:
            if os.path.exists(source):
                os.remove(source)
        except Exception:
            pass
        return None


def _pregenerate_codegen_targets(source_files):
    """
    Identify generator targets among *source_files*, run their generators
    in parallel, and register each result with redo-done.

    Returns the list of successfully pre-generated file paths.  The caller
    should exclude these from its redo-ifchange call since redo-done has
    already recorded them.
    """
    from database.generator_database import generator_database
    from database.database import DATABASE_MODE
    from util import redo

    if not source_files:
        return []

    # Step 1: Build work list
    # Query the generator database to figure out which source files are
    # produced by a code generator and can be pre-generated in-process.
    work_items = []
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
                if os.path.isfile(source):
                    continue

                module_name, class_name, file_name, input_filename = gen_info

                # Input model must already exist on disk; if not, redo will
                # need to build it first via the normal .do path.
                if not os.path.isfile(input_filename):
                    continue

                work_items.append((
                    source, module_name, class_name, file_name, input_filename
                ))
    except Exception:
        # If we can't even open the generator database for some reason.
        # Fall back gracefully. All files will go through the normal
        # redo-ifchange path.
        return []

    if not work_items:
        return []

    # Step 2: Generate outputs in parallel and register with redo-done
    # Each worker process loads its own generator and model objects. The
    # on-disk model_cache.db is safe for concurrent reads; occasional
    # cache-miss writes are serialized by filelock inside the DB layer.
    num_workers = multiprocessing.cpu_count()
    if num_workers > 1 and len(work_items) > 1:
        with multiprocessing.Pool(processes=num_workers) as pool:
            results = pool.map(_generate_one, work_items)
    else:
        results = [_generate_one(item) for item in work_items]

    # Step 3: Register with redo-done in serial
    # redo-done is a subprocess that updates redo's build database. We run
    # these serially to avoid concurrent writes to redo's database.
    pregenerated = []
    for result in results:
        if result is not None:
            source, all_deps = result
            redo.redo_done(source, all_deps)
            pregenerated.append(source)

    return pregenerated


def pregenerate_and_redo_done(source_files):
    """
    Pre-generate source file targets in-process (in parallel), then
    redo-ifchange the remaining (non-pre-generated) sources. This is
    the main entry point used by build_object.py wherever it would
    normally call redo.redo_ifchange on source files.
    """
    from util import redo

    if not source_files:
        return

    # Deduplicate to avoid redundant generation or redo-ifchange calls.
    source_files = list(dict.fromkeys(source_files))

    # If DISABLE_PREGEN is set then we fallback to the safe, slow
    # redo-ifchange of all source files.
    if os.environ.get("DISABLE_PREGEN"):
        redo.redo_ifchange(source_files)
        return

    # Generate what we can in-process. These targets are registered
    # with redo-done and don't need redo-ifchange.
    pregenerated = set(_pregenerate_codegen_targets(source_files))

    # Everything else (non-generator sources, existing files that need
    # staleness checks, generators we couldn't run) goes through
    # redo-ifchange.
    remaining = [s for s in source_files if s not in pregenerated]
    if remaining:
        redo.redo_ifchange(remaining)
