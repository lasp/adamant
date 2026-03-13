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


# ---------------------------------------------------------------------------
# Worker function — runs in a child process via multiprocessing.Pool
# ---------------------------------------------------------------------------

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

        # Each worker creates its own generator instance.  The generator's
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

        # Generators write to stdout (redo convention).  Capture the output
        # and write it to the target file ourselves.
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()
        try:
            generator.generate(input_filename)
        finally:
            sys.stdout = old_stdout

        with open(source, "w") as f:
            f.write(captured.getvalue())

        # Build the full dep list for redo-done: input model file, the
        # generator's Python module, plus any generator-declared deps.
        generator_module_file = sys.modules[generator.__module__].__file__
        all_deps = [input_filename, generator_module_file]
        if dependencies:
            all_deps.extend(dependencies)

        return (source, all_deps)

    except Exception as e:
        # Log so the failure is visible, then clean up any partial output.
        # The file will fall through to redo-ifchange in the caller.
        sys.stderr.write(
            "  pregen: FAILED {} — {}\n".format(os.path.basename(source), e)
        )
        try:
            if os.path.exists(source):
                os.remove(source)
        except Exception:
            pass
        return None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def pregenerate_codegen_targets(source_files):
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

    # --- Phase 1: build work list (serial, fast) --------------------------
    # Query the generator database to figure out which source files are
    # produced by a code generator and can be pre-generated in-process.
    work_items = []
    try:
        with generator_database(mode=DATABASE_MODE.READ_ONLY) as db:
            for source in source_files:
                try:
                    gen_info = db.get_generator(source)
                except KeyError:
                    continue

                # File already exists — let redo-ifchange handle staleness.
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
        return []

    if not work_items:
        return []

    # --- Phase 2: generate in parallel ------------------------------------
    # Each worker process loads its own generator and model objects.  The
    # on-disk model_cache.db is safe for concurrent reads; occasional
    # cache-miss writes are serialized by filelock inside the DB layer.
    num_workers = multiprocessing.cpu_count()
    if num_workers > 1 and len(work_items) > 1:
        with multiprocessing.Pool(processes=num_workers) as pool:
            results = pool.map(_generate_one, work_items)
    else:
        results = [_generate_one(item) for item in work_items]

    # --- Phase 3: register with redo-done (serial) ------------------------
    # redo-done is a subprocess that updates redo's build database.  We run
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
    Pre-generate what we can in parallel, then redo-ifchange the rest.

    Drop-in replacement for redo.redo_ifchange() on source file lists.
    """
    from util import redo

    if not source_files:
        return

    source_files = list(dict.fromkeys(source_files))  # deduplicate

    pregenerated = set(pregenerate_codegen_targets(source_files))

    remaining = [s for s in source_files if s not in pregenerated]
    if remaining:
        redo.redo_ifchange(remaining)
