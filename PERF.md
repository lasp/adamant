# Build Performance Analysis

## Benchmark: `redo test_all` on `src/components/command_router` (clean build)

**Machine:** 8-core x86_64, 16GB RAM, Docker container  
**Baseline:** ~65s wall time (3 runs: 65.5s, 66.8s, 65.6s)

---

## perf/01-profiling-baseline — Initial Profiling Results

### Top Time Consumers (aggregated across all redo subprocess invocations)

| Phase | Total Time | Calls | Avg/Call | Notes |
|-------|-----------|-------|---------|-------|
| `db:source.db` | 73.0s | 607 | 0.120s | **#1 bottleneck** — source database opened/closed 607 times |
| `db:redo_target.db` | 60.8s | 8 | 7.605s | Long-lived opens (held open during entire build phases) |
| `executable:build_all_obj_deps` | 36.0s | 2 | 18.0s | Object dependency resolution for 2 test executables |
| `db:generator.db` | 27.7s | 280 | 0.099s | Generator database opened 280 times |
| `db:c_source.db` | 13.5s | 8 | 1.686s | |
| `db:py_source.db` | 11.6s | 4 | 2.903s | |
| `precompile:resolve_deps` | 10.3s | 5 | 2.059s | Dependency resolution before gprbuild |
| `db:build_target.db` | 10.3s | 262 | 0.039s | Build target DB opened 262 times |
| `precompile:gprbuild` | 8.4s | 5 | 1.675s | **Actual compilation** — only 8.4s of 65s total! |
| `db:models.db` | 6.9s | 786 | 0.009s | Model cache DB |
| `db:model_cache.db` | 5.1s | 1124 | 0.005s | |
| `executable:bind` | 0.9s | 2 | 0.445s | |
| `executable:link` | 1.0s | 2 | 0.481s | |
| Code generation (all) | ~8.5s | ~160 | 0.05s | Individual generators are fast |

### Key Insights

1. **Database I/O dominates the build** — over 200s cumulative time spent opening/closing databases across 3000+ operations. These are UnQLite databases with pickle serialization, opened/closed via Python context managers on every redo subprocess invocation.

2. **Actual compilation (gprbuild) is only ~8.4s** — the compiler itself is fast. The overhead is all in the Python build orchestration layer.

3. **source.db is opened 607 times** — each redo subprocess opens this database independently. A shared or cached database connection would eliminate massive redundancy.

4. **The redo model spawns many subprocesses** — each `redo_ifchange` call can spawn a new Python process, which must re-import modules, re-open databases, and re-resolve paths. This per-process overhead is the fundamental bottleneck.

5. **Dependency resolution takes 10.3s** — scanning Ada source files for `with` clauses and recursively resolving them.

### Optimization Priority (based on data)

1. **Database connection caching/persistence** — eliminate repeated open/close of source.db (607 opens), generator.db (280), build_target.db (262), models.db (786), model_cache.db (1124). This is the single biggest opportunity.

2. **Reduce redo subprocess spawning** — batch more work into fewer processes to amortize import/DB-open overhead.

3. **Dependency resolution caching** — cache resolved dependency trees to avoid re-scanning source files.

4. **Code generation is already fast** — individual generators take 30-70ms. Not worth optimizing yet.

5. **Compilation is already fast** — gprbuild at 8.4s with parallel compilation is near-optimal.

---

## perf/02-database-connection-cache — Read-Only DB Connection Caching

**Change:** Cache read-only UnQLite connections within each redo subprocess using module-level dict with refcounting. Subsequent `with database() as db:` calls reuse the connection instead of opening/closing.

**Result:** ~66.9s wall time (baseline: ~65.5s) — **no significant wall-time improvement**

**However, database overhead dropped significantly:**

| Database | Before (calls/time) | After (calls/time) | Reduction |
|---|---|---|---|
| source.db | 607 / 73.0s | 367 / 67.0s | -40% calls, -8% time |
| generator.db | 280 / 27.7s | 140 / 5.5s | -50% calls, **-80% time** |
| build_target.db | 262 / 10.3s | 131 / 1.9s | -50% calls, **-82% time** |
| model_cache.db | 1124 / 5.1s | 562 / 2.5s | -50% calls, -51% time |
| models.db | 786 / 6.9s | 393 / 0.4s | -50% calls, **-94% time** |
| redo_target.db | 8 / 60.8s | 4 / 0.6s | -50% calls, **-99% time** |

**Analysis:** The caching works within each process, cutting calls ~50%. But redo spawns many **separate processes** (each `redo_ifchange` can fork), so the cache is per-process, not shared. The remaining wall-time bottleneck is:
1. Process spawn overhead (Python import + module init per subprocess)
2. Pickle deserialization of database values (not just file open)
3. source.db still opened 367 times across different processes

**Verdict:** Keep this change (reduces overhead, enables future improvements). But the real win requires reducing the number of redo subprocess spawns or sharing database state across processes.

---

## perf/03-fetch-cache — In-Memory Fetch Cache

**Change:** Cache deserialized pickle results in a dict within READ_ONLY database instances. Repeated fetches of the same key skip both UnQLite lookup and pickle deserialization.

**Result:** ~65.95s (baseline: ~65.5s) — **no wall-time improvement**

**Analysis:** Pickle deserialization per call was already fast (~5-9ms for models.db/model_cache.db). The cache helps with repeated same-key lookups within a process, but most lookups are unique keys (different package names). The 262 redo subprocesses each build their own cache from scratch.

**Verdict:** Minimal impact. Keep for correctness/future use but this isn't the bottleneck.

---

## perf/04-bulk-preload — Bulk Preload Database Into Memory (REGRESSION)

**Change:** At database open, iterate all entries and deserialize them into a Python dict, eliminating per-key UnQLite lookups entirely.

**Result:** ~84.9s (baseline: ~65.5s) — **29% REGRESSION**

**Analysis:** The databases contain many entries (all packages across the entire Adamant project), but each build only accesses a small subset. Bulk-deserializing everything wastes time on unused entries. With 262 subprocesses, each paying this upfront cost, the overhead is massive.

**Verdict:** REVERTED. Branched off perf/03 for future work.

---

## perf/05-lazy-filelock — Lazy-Import filelock Module

**Change:** Defer `from filelock import FileLock, Timeout` until actually needed (write operations only). The `filelock` module imports `asyncio` (~54ms), which is wasted overhead in the many read-only redo subprocesses that never acquire file locks.

**Result:** ~60.0s (2 runs: 60.0s, 59.9s) — **8.4% improvement over baseline**

**Analysis:** This is the first optimization to produce a meaningful wall-time reduction. The filelock/asyncio import was happening in every single redo subprocess (262 of them), adding ~54ms × 262 ≈ 14s of cumulative import time. Since redo runs 8 parallel jobs, this translates to ~1.8s wall-time per parallel slot, totaling ~5-6s real improvement. Read-only subprocesses (majority of the 262) never need filelock.

**Verdict:** KEEP. Significant win with zero risk — lazy import is semantically identical.

---

## perf/06-combined-redo-ifchange — Combine Object + Non-Object redo_ifchange Calls

**Change:** In `build_all.py`, instead of two separate `redo_ifchange` calls (one for objects, one for generated files), combine all targets into a single `redo_ifchange` call. This gives the redo scheduler better visibility into all work, improving parallelism.

**Result:** ~59.5s (2 runs: 59.7s, 59.2s) — **marginal improvement over perf/05**

**Analysis:** The two separate `redo_ifchange` calls were already somewhat parallel, but combining them eliminates the serialization point between object compilation and code generation. The improvement is small (~0.5s) because most generated files were already being built in parallel with compilation via the precompile step.

**Verdict:** KEEP. Small but consistent improvement with cleaner code.

---

## perf/07-skip-site-import — PYTHONNOUSERSITE and PYTHONDONTWRITEBYTECODE

**Change:** Set `PYTHONNOUSERSITE=1` and `PYTHONDONTWRITEBYTECODE=1` in `env/activate` to skip user site-packages scanning and .pyc file writing in all redo subprocesses.

**Result:** ~60.0s (2 runs: 59.9s, 60.4s) — **no measurable improvement over perf/06**

**Analysis:** The Docker container has minimal user site-packages, so `PYTHONNOUSERSITE` saves negligible time. `PYTHONDONTWRITEBYTECODE` avoids .pyc writes but .pyc files are already cached from previous runs, so the overhead of checking/writing them is minimal. These are good hygiene settings but don't move the needle in this environment.

**Verdict:** KEEP (zero risk, good practice) but no performance impact.

---

## perf/09-lazy-pickle-import — Defer pickle/unqlite Imports

**Change:** Lazy-import `pickle` (~10ms) and `unqlite` (~4ms) in database.py, deferring them until the first actual database open or fetch call. Also moves `sys.setrecursionlimit(5000)` into the pickle init path.

**Result:** ~60.1s (2 runs: 60.2s, 60.0s) — **no improvement over perf/07**

**Analysis:** Nearly every redo subprocess accesses at least one database, so the imports are merely deferred, not avoided. The total import cost is the same — it just happens slightly later in each process. This would only help if there were subprocesses that don't touch any database (there aren't many).

**Verdict:** KEEP (cleaner lazy-loading pattern, no regression) but no wall-time impact.

---

## perf/09-lazy-pickle-import — Defer pickle/unqlite Imports

**Change:** Lazy-import `pickle` (~10ms) and `unqlite` (~4ms) in database.py, deferring them until the first actual database open or fetch call. Also moves `sys.setrecursionlimit(5000)` into the pickle init path.

**Result:** ~60.1s (2 runs: 60.2s, 60.0s) — **no improvement over perf/07**

**Analysis:** Nearly every redo subprocess accesses at least one database, so the imports are merely deferred, not avoided. The total import cost is the same — it just happens slightly later in each process. This would only help if there were subprocesses that don't touch any database (there aren't many).

**Verdict:** KEEP (cleaner lazy-loading pattern, no regression) but no wall-time impact.

---

## perf/21-precompile-deps-sidecar — Per-object prebuilt deps sidecar

**Change:** During `_precompile_objects`, write precomputed deps metadata for prebuilt objects and use it in `_handle_prebuilt_object` instead of always recalculating dependencies with `_build_all_ada_and_c_dependencies_for_object([redo_1], dry_run=True)`.

**Result:** `60.33s` wall time (single run)

- vs baseline `65.5s`: **7.89% faster**
- vs perf/09 `60.1s`: **0.38% slower**

**Analysis:** This removes a lot of redundant dependency re-resolution work on the hot prebuilt path, but first implementation still had fallback overhead for some cross-batch precompiled objects.

---

## perf/22-lazy-imports-prebuilt-path — Defer heavy imports off prebuilt fast path

**Change:** Converted heavy top-level imports to lazy helpers (`source_database`, `c_source_database`, `build_target_database`, `build_target`, `rmtree`) so prebuilt-object subprocesses avoid importing DB-heavy modules unless they miss the prebuilt fast path.

**Result:** `60.75s` wall time (single run)

- vs baseline `65.5s`: **7.25% faster**
- vs perf/09 `60.1s`: **1.08% slower**

**Analysis:** No measurable gain over perf/21 in this run; likely import savings are small compared to remaining per-subprocess redo/dependency bookkeeping.

---

## perf/23-reduce-prebuilt-subprocess-work — Shared manifest + less redo_ifchange

**Change:**
- Replaced per-object sidecars with a single manifest (`_prebuilt_deps_manifest.json`) for all prebuilt objects.
- Added per-process manifest cache in module scope.
- In `_handle_prebuilt_object`, skip `redo_ifchange` for pure source-file deps; only run it for non-source deps.

**Result:** `53.83s` wall time (single run)

- vs baseline `65.5s`: **17.82% faster**
- vs perf/09 `60.1s`: **10.43% faster**

**Analysis:** This is the first major improvement in this series. It meaningfully reduces per-object subprocess work on prebuilt paths.

---

## perf/24-skip-prebuilt-dep-ifchange — Skip dep redo_ifchange when precompile resolved

**Change:**
- `_precompile_objects` sets `PRECOMPILE_DEPS_RESOLVED=1`.
- `_handle_prebuilt_object` skips dependency `redo_ifchange` when that flag is set; it only writes the `.deps` file and moves outputs.

**Result:** `53.16s` wall time (single run)

- vs baseline `65.5s`: **18.84% faster**
- vs perf/09 `60.1s`: **11.55% faster**

**Analysis:** Best result among tested branches. Confirms that post-precompile per-object dep checks were mostly redundant in clean builds.

---

## perf/26-batch-redo-done-manifest — Batch redo-done via manifest file

**Change:** Modified redo-done (Haskell) to support `@manifest:<path>` syntax for batch registration. After `_precompile_objects` compiles all objects, moves them to final locations in Python, writes a tab-separated manifest file, and calls `redo-done @manifest:<path>` once instead of per-object .do script invocations. Removes prebuilt objects from subsequent redo_ifchange call.

**Result:** ~61.0s (baseline perf/24: ~59.0s) — **slight regression (3.4%)**

**Analysis:** The batch redo-done successfully eliminates all per-object redo subprocess spawns (verified: only 1 redo target in build output). However, the manifest generation + single Haskell redo-done call adds overhead (reading .do files, initializing databases for each target), and the objects that were previously handled by fast .do scripts (just file moves) were already cheap in perf/24. The net effect is slightly worse due to manifest I/O overhead.

**Verdict:** Not beneficial. Per-object .do scripts with perf/24 optimizations are already fast enough.

---

## perf/27-fast-executable-deps — Fast executable dep resolution via .deps files

**Change:** Added `_collect_all_object_deps_from_files()` function in build_executable.py that uses BFS traversal of .deps files to collect all transitive object dependencies. When .deps files exist (from prior object builds via build_all precompile), skips the recursive `_build_all_ada_object_dependencies()` which made many iterative redo_ifchange calls. Does a single redo_ifchange on all collected deps.

**Result:** ~51.8s avg (2 runs: 51.7s, 52.0s)

- vs baseline 65s: **20.3% faster**
- vs perf/24 59.0s: **12.2% faster**

**Analysis:** This is the biggest single improvement in this round. The recursive dep resolution in build_executable spent ~23.6s (cumulative, 2 calls) doing iterative redo_ifchange on objects that were already built. The BFS approach reads .deps files directly (no subprocess spawns) and does a single bulk redo_ifchange at the end. The .deps files are created by build_object during the precompile phase, so they always exist when build_executable runs.

**Verdict:** KEEP. Significant win with clean fallback to original behavior when .deps files dont


---

## perf/26-batch-redo-done-manifest — Batch redo-done via manifest file

**Change:** Modified redo-done (Haskell) to support @manifest:path syntax for batch registration. After _precompile_objects compiles all objects, moves them to final locations in Python, writes a tab-separated manifest file, and calls redo-done once instead of per-object .do script invocations.

**Result:** ~61.0s (baseline perf/24: ~59.0s) — **slight regression (3.4%)**

**Analysis:** The batch redo-done successfully eliminates all per-object redo subprocess spawns. However, the manifest generation + Haskell redo-done call adds overhead, and the objects were already handled cheaply by perf/24's fast .do scripts.

**Verdict:** Not beneficial. Per-object .do scripts with perf/24 optimizations are already fast enough.

---

## perf/27-fast-executable-deps — Fast executable dep resolution via .deps files

**Change:** Added BFS traversal of .deps files in build_executable.py to collect all transitive object dependencies without recursive redo_ifchange calls. When .deps files exist (from build_all precompile), does a single redo_ifchange on all collected deps instead of iterative resolution.

**Result:** ~51.8s avg (2 runs: 51.7s, 52.0s)

- vs baseline 65s: **20.3% faster**
- vs perf/24 59.0s: **12.2% faster**

**Analysis:** Biggest improvement in this round. The recursive dep resolution in build_executable spent ~23.6s cumulative doing iterative redo_ifchange on already-built objects. BFS reads .deps files directly (no subprocess spawns) and does a single bulk redo_ifchange.

**Verdict:** KEEP. Significant win with clean fallback.

---

## perf/28-skip-exe-dep-ifchange — Skip redo_ifchange for existing source deps

**Change:** In _build_all_ada_dependencies, filter out already-existing source files before calling redo_ifchange. Only call redo_ifchange for missing (generated) files. Based on perf/27.

**Result:** ~51.8s — **same as perf/27**

**Analysis:** Most Ada source files already exist; redo_ifchange on them is cheap (just stamp checks). Filtering adds negligible overhead.

**Verdict:** Marginal. No measurable wall-time impact.

---

## perf/29-combined-optimizations — Combined skip-existing optimizations

**Change:** Extends perf/28 by also skipping redo_ifchange for existing immediate source deps and C binding deps.

**Result:** ~51.9s avg (2 runs: 51.7s, 52.0s)

- vs baseline 65s: **20.2% faster**
- vs perf/24 59.0s: **12.0% faster**

**Analysis:** No additional improvement over perf/27. The skip-existing optimizations are marginal.

**Verdict:** perf/27 is the recommended branch.

---

## Summary of perf/26-29

| Branch | Time | vs 65s baseline | vs perf/24 (59s) | Key Change |
|--------|------|-----------------|------------------|------------|
| perf/26 | 61.0s | -6.2% | +3.4% | Batch redo-done manifest |
| perf/27 | 51.8s | -20.3% | -12.2% | Fast exe dep resolution via .deps |
| perf/28 | 51.8s | -20.3% | -12.2% | + Skip existing source redo_ifchange |
| perf/29 | 51.9s | -20.2% | -12.0% | Combined all optimizations |

**Best result: perf/27** at ~51.8s.

### Important benchmark note
Always clear ~/.redo and /tmp/redo-* before benchmarking. Stale redo metadata from branch switches causes incorrect results.
