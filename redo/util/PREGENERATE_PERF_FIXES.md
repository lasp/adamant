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

### Baseline (no fixes)

| Phase         | Time   | % of Total |
|---------------|--------|------------|
| depends_on()  | 90.9s  | 67%        |
| generate()    | 37.9s  | 28%        |
| redo-done     | 7.5s   | 5%         |
| DB lookup     | 0.023s | ~0%        |
| Write file    | 0.145s | ~0%        |
| Get instance  | 0.052s | ~0%        |
| **Total**     | 136.6s |            |

130 files pregenerated, 0 failed.

### After Fix 1 (single DB connection)

| Phase         | Time   | % of Total |
|---------------|--------|------------|
| depends_on()  | 80.3s  | 61%        |
| generate()    | 42.6s  | 32%        |
| redo-done     | 8.0s   | 6%         |
| **Total**     | 131.3s |            |

133 files pregenerated, 0 failed. ~12% reduction in depends_on().

### After Fix 1 + Fix 2 (generate before depends_on)

| Phase         | Time   | % of Total |
|---------------|--------|------------|
| depends_on()  | 2.7s   | 2%         |
| generate()    | 81.8s  | 60%        |
| redo-done     | 7.3s   | 5%         |
| **Total**     | 136.8s |            |

128 files pregenerated, 16 failed (fall through to redo-ifchange).
depends_on() reduced by 97%. Model loading cost now correctly attributed to generate().

---

## Fix 1: Keep model_cache_database open across calls — APPLIED

**Commit**: `5bccb730`
**File**: `gen/models/base.py`, function `load_from_cache()` (line 125)

Refactored all inner helper functions to accept a `db` parameter. One `with
model_cache_database()` wraps all READ_ONLY operations. Only
`mark_model_cached_this_session()` opens a separate READ_WRITE connection.

Reduces 50-100 DB opens to 1-2 per model load. Measured ~12% reduction in
depends_on() time.

---

## Fix 2: Move depends_on() after generate() — APPLIED

**Commit**: `c1154993`
**File**: `redo/util/pregenerate.py`, function `pregenerate_codegen_targets()`

Both `depends_on()` and `generate()` call `model_object()`, which caches the
result. By running `generate()` first, the expensive pickle deserialization
happens once and `depends_on()` afterwards is free (cached lookup).

16 files that fail generation (due to missing deps) fall through to redo-ifchange
which handles them through the normal build path.

---

## Fix 3: Cache database connection at module level (MEDIUM IMPACT, EASY)

**File**: `redo/util/pregenerate.py`

Open the model_cache_database once at the start of `pregenerate_codegen_targets()` and
reuse it for all files in the batch, rather than letting each `depends_on()` →
`model_object()` → `load_from_cache()` open its own connections.

This would require threading the DB handle through the generator API, which is more
invasive than Fix 1.

---

## Fix 4: Reduce pickle size via __getstate__/__setstate__ (HIGH IMPACT, HARD)

**Files**: `gen/models/base.py`, `gen/models/component.py`, `gen/models/assembly.py`

### Current state

The entire model object graph is pickled with NO `__getstate__`/`__setstate__`:
- All submodel objects (commands, events, faults, parameters, packets, data_products)
- All type model objects (recursive — types containing types)
- All enum models
- Circular references (assembly ↔ component ↔ submodel)
- Full `self.data` dictionary (parsed YAML)
- A component model can be 5-10MB pickled
- Assembly models are even larger (reference all component models)

### Options

**A. Add __getstate__ to exclude reconstructable data:**
- Exclude nested model objects that can be re-loaded from their own cache entries
- Exclude the raw `self.data` dict (can be re-parsed from YAML)
- Keep only essential computed fields + dependency list
- Risk: subtle bugs from incomplete reconstruction

**B. Store dependencies as a separate lightweight DB key:**
- In `model_cache_database.store_model()`, also store
  `model_file + "_deps@@"` → `model.get_dependencies()`
- Add `get_model_dependencies(filename)` that fetches just the dep list
- Pregenerate.py (or `depends_on()`) can read deps without full deserialization
- Low risk, targeted improvement

**C. Store submodels as separate DB entries:**
- Instead of pickling the full object graph, pickle each submodel independently
- Reconstruct references on load
- Biggest potential win but most invasive change
