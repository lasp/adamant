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
