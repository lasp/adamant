# Pregeneration Performance Fix Proposals

## Problem

`depends_on()` accounts for 67% of pregeneration time (90.9s out of 136.6s).

Root cause: `load_from_cache()` in `gen/models/base.py` opens and closes the 334MB
`model_cache.db` unqlite database **4 + 2N times per model** (N = dependency count).
For assembly models with dozens of dependencies, this means 50-100+ open/close cycles
of a 334MB file per model load.

Mac Docker is 40% slower because overlay filesystem sits on a virtual disk image
(`Docker.raw`), so every `open()` + `mmap()` of the 334MB file pays virtualization
overhead. On native Linux Docker, overlay is native kernel I/O.

## Profiling Data (from instrumented clean build)

| Phase         | Time   | % of Total |
|---------------|--------|------------|
| depends_on()  | 90.9s  | 67%        |
| generate()    | 37.9s  | 28%        |
| redo-done     | 7.5s   | 5%         |
| DB lookup     | 0.023s | ~0%        |
| Write file    | 0.145s | ~0%        |
| Get instance  | 0.052s | ~0%        |
| **Total**     | 136.6s |            |

130 files pregenerated out of 207 checked.

---

## Fix 1: Keep model_cache_database open across calls (HIGH IMPACT, EASY)

**File**: `gen/models/base.py`, function `load_from_cache()` (line 125)

Instead of opening/closing the DB for every sub-query in `load_from_cache()`, open
it once in READ_ONLY mode and pass the handle through all the helper functions:

```python
def load_from_cache(cls, filename):
    with model_cache_database() as db:
        # All reads use same open handle — 1 open instead of 4+2N
        if is_model_cached_this_session(db, filename):
            return do_load_from_cache(db, filename)
        if were_new_submodels_created(db, filename):
            return None
        if is_cached_model_up_to_date(db, filename):
            model = do_load_from_cache(db, filename)
            if model:
                for dep in model.get_dependencies():
                    if not is_cached_model_up_to_date(db, dep) or \
                           is_model_cached_this_session(db, dep):
                        return None
    # Single READ_WRITE open for the mark
    mark_model_cached_this_session(filename)
    return model
```

This reduces 50-100 DB opens to 1-2 per model load.

---

## Fix 2: Skip depends_on() during pregeneration entirely (HIGH IMPACT, EASY)

**File**: `redo/util/pregenerate.py`, function `pregenerate_codegen_targets()`

The `depends_on()` result is only used to check if all dependency files exist on disk
(lines 154-159). For a clean build, dependencies often won't exist yet and files get
skipped. Options:

- Store dependency info in the generator_database alongside generator info, avoiding
  the need to load the full model
- Skip the `depends_on()` check during pregeneration and let redo handle stale deps
  via the `redo-done` registration (deps are still registered, just not pre-validated)
- Only call `depends_on()` for non-assembly generators (component-level models are
  fast, assembly models are the bottleneck)

---

## Fix 3: Cache database connection at module level (MEDIUM IMPACT, EASY)

**File**: `redo/util/pregenerate.py`

Open the model_cache_database once at the start of `pregenerate_codegen_targets()` and
reuse it for all files in the batch, rather than letting each `depends_on()` →
`model_object()` → `load_from_cache()` open its own connections.

This would require threading the DB handle through the generator API, which is more
invasive than Fix 1.
