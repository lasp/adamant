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
