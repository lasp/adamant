"""
In-process codegen pre-generation to avoid redo subprocess overhead entirely.

v4: Cache generator instances across calls to avoid redundant imports.
"""
import os
import sys
import io

# Module-level cache for generator instances
_generator_cache = {}  # (module_name, class_name, file_name) -> generator_instance


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
    Given source files about to be redo_ifchange'd, identify generator
    targets, pre-generate them in-process, and return the list of
    successfully pre-generated targets (to exclude from redo_ifchange).
    """
    from database.generator_database import generator_database
    from database.database import DATABASE_MODE
    from util import filesystem

    if not source_files:
        return []

    pregenerated = []
    try:
        with generator_database(mode=DATABASE_MODE.READ_ONLY) as db:
            for source in source_files:
                try:
                    gen_info = db.get_generator(source)
                except KeyError:
                    continue

                # Already exists on disk? Skip (incremental build)
                if os.path.isfile(source):
                    pregenerated.append(source)
                    continue

                module_name = gen_info[0]
                class_name = gen_info[1]
                file_name = gen_info[2]
                input_filename = gen_info[3]

                if not os.path.isfile(input_filename):
                    continue

                try:
                    generator = _get_generator_instance(module_name, class_name, file_name)

                    # Check deps exist
                    try:
                        dependencies = generator.depends_on(input_filename)
                    except Exception:
                        continue
                    if dependencies:
                        if isinstance(dependencies, str):
                            dependencies = [dependencies]
                        skip = False
                        for dep in dependencies:
                            if not os.path.isfile(dep):
                                skip = True
                                break
                        if skip:
                            continue

                    # Make output dir
                    dirname = os.path.dirname(source)
                    filesystem.safe_makedir(dirname)

                    # Capture stdout
                    old_stdout = sys.stdout
                    sys.stdout = captured = io.StringIO()
                    try:
                        generator.generate(input_filename)
                    finally:
                        sys.stdout = old_stdout

                    content = captured.getvalue()
                    with open(source, "w") as f:
                        f.write(content)

                    pregenerated.append(source)
                except Exception:
                    # Failed — let redo handle it
                    try:
                        if os.path.exists(source):
                            os.remove(source)
                    except Exception:
                        pass
    except Exception:
        pass

    return pregenerated


def pregenerate_and_ifchange(source_files, dry_run=False):
    """
    Pre-generate codegen targets in-process, then redo_ifchange
    only the non-generator sources.
    """
    from util import redo

    if not source_files:
        return
    if dry_run:
        return

    pregenerated = set(pregenerate_codegen_targets(source_files))
    remaining = [s for s in source_files if s not in pregenerated]
    if remaining:
        redo.redo_ifchange(remaining)
