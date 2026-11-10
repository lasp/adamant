"""
Parallel in-process code pre-generation.

Normally each generated source file is built by redo spawning a subprocess
that runs build_via_generator.  For large builds this means hundreds of
Python subprocess invocations just for codegen.  This module short-circuits
that by running generators directly in forked worker processes, writing
output files to disk, and registering them with redo-done so redo knows
they are up-to-date.

Each worker process creates its own generator instances and in-memory model
cache.  The shared on-disk model_cache.db supports concurrent readers
(READ_ONLY requires no lock) and serializes the occasional cache-miss write
via file locks, so no additional synchronization is needed here.

Each worker registers its generated file with redo-done immediately after
writing it.  Redo's database handles concurrent writes safely.

Generation is scheduled by memory.  Each target runs in its own short-lived
forked process, a worker, started only while the memory it is expected to
need is available: free memory, the lesser of what the kernel reports and
what the cgroup still allows, less the growth still ahead of the running
workers, must cover one more worker plus the headroom.  A worker that
exits by signal counts as failed, and its target goes through redo-ifchange
like any other failed target.  Where memory cannot be read, the core count
alone bounds the workers.

Terms used throughout:

  worker         a forked process generating one target.
  model type     the type of a target's input model (assembly, component,
                 record, ...), which is what sets how much its generator
                 loads.  Growth is kept per model type.
  parent size    the resident size of the process the workers are forked
                 from, read when the scheduler starts.  A worker starts as
                 a copy of it.
  growth         the bytes a worker reaches beyond the parent size: the
                 most any worker of the model type has grown, which means
                 the same in every build step whatever its parent size.
  expected size  the bytes a worker of a model type is expected to reach:
                 the parent size plus the model type's growth, or a
                 placeholder until one worker of the model type has
                 finished.
  headroom       memory kept free for everything besides the workers: a
                 tenth of total memory, the lesser of what the kernel
                 reports and the cgroup limit, kept between 512 MiB and
                 2 GiB.
  measured       a model type one worker has finished for, so its growth is
                 a measurement.
  probe          the one worker of an unmeasured model type allowed to run
                 at a time, which measures the model type for the rest.

Every size is in bytes.  The measured growth is saved to
build/pregenerate_worker_growth.json in the directory the top-level redo
was run from, so each batch of every step of a build starts from what the
earlier batches measured.

Setting DISABLE_PREGEN in the environment skips pre-generation entirely and
every source goes through redo-ifchange.
"""
import io
import json
import multiprocessing
import multiprocessing.connection
import os
import sys


# Worker - runs in a forked child process
def _generate_one(work_item):
    """
    Generate a single source file in a worker process.

    Returns (source_path, dep_list) on success, or None on failure.
    Failures are logged to stderr and the caller falls back to the
    normal redo-ifchange path for that file.
    """
    source, module_name, class_name, file_name, input_filename = work_item
    partial = "%s.%d.tmp" % (source, os.getpid())

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

        with open(partial, "w") as f:
            f.write(captured.getvalue())
        os.replace(partial, source)

        # Build the full dep list for redo-done including input model file,
        # generator's Python module, plus any generator-declared deps.
        generator_module_file = sys.modules[generator.__module__].__file__
        all_deps = [input_filename, generator_module_file]
        if dependencies:
            all_deps.extend(dependencies)

        from util import redo
        redo.redo_done(source, all_deps)

        return source

    except Exception:
        # Generation failed. Clean up any partial output and let
        # redo handle this target through its normal .do script path.
        for leftover in (source, partial):
            try:
                if os.path.exists(leftover):
                    os.remove(leftover)
            except Exception:
                pass
        return None


MIB = 1024 * 1024
GIB = 1024 * MIB
MINIMUM_HEADROOM = 512 * MIB
MAXIMUM_HEADROOM = 2 * GIB


def system_memory(meminfo="/proc/meminfo", cgroup="/sys/fs/cgroup"):
    """
    (total, available) bytes: the lesser of what the kernel reports and what
    the cgroup allows, for the memory as a whole and for what a new worker
    could use now.  Either is None when unreadable (not Linux, or no /proc),
    in which case the scheduler falls back to the core count alone.  The
    paths are parameters so a test can point them at its own files.
    """
    def read_meminfo():
        """(MemTotal, MemAvailable) in bytes, each None when unreadable."""
        fields = {}
        try:
            with open(meminfo) as f:
                for line in f:
                    name, value = line.split(":", 1)
                    fields[name] = int(value.split()[0]) * 1024
        except (OSError, ValueError, IndexError):
            pass
        return fields.get("MemTotal"), fields.get("MemAvailable")

    def read_cgroup():
        """
        (limit, available) bytes under the cgroup memory limit, or None when
        there is no limit or it cannot be read.  Checks the unified (v2)
        hierarchy first, then the legacy (v1) memory controller, which
        reports no limit as a huge number near the page-aligned maximum.  The
        usage leaves out the inactive file cache when memory.stat reports it.
        """
        for limit_name, usage_name, stat_name, cache_key in (
            ("memory.max", "memory.current", "memory.stat", "inactive_file"),
            ("memory/memory.limit_in_bytes", "memory/memory.usage_in_bytes", "memory/memory.stat", "total_inactive_file"),
        ):
            try:
                with open(os.path.join(cgroup, limit_name)) as f:
                    limit = f.read().strip()
                with open(os.path.join(cgroup, usage_name)) as f:
                    usage = int(f.read().strip())
            except (OSError, ValueError):
                continue
            if limit == "max":
                return None
            try:
                limit = int(limit)
            except ValueError:
                return None
            if limit >= 2 ** 60:
                return None
            cache = 0
            try:
                with open(os.path.join(cgroup, stat_name)) as f:
                    for line in f:
                        key, value = line.split()
                        if key == cache_key:
                            cache = int(value)
                            break
            except (OSError, ValueError):
                pass
            return limit, max(0, limit - max(0, usage - cache))
        return None

    def least(*readings):
        known = [r for r in readings if r is not None]
        return min(known) if known else None

    total, available = read_meminfo()
    cgroup_reading = read_cgroup()
    if cgroup_reading is not None:
        total = least(total, cgroup_reading[0])
        available = least(available, cgroup_reading[1])
    return total, available


def headroom_for(total):
    """The headroom for this much total memory, or its minimum when unknown."""
    if total is None:
        return MINIMUM_HEADROOM
    return min(MAXIMUM_HEADROOM, max(MINIMUM_HEADROOM, total // 10))


def growth_file():
    """
    Where the measured growth is saved: the build directory of the directory
    the top-level redo was run from, or None outside of redo.
    """
    root = os.environ.get("REDO_INIT_PATH")
    return os.path.join(root, "build", "pregenerate_worker_growth.json") if root else None


def load_growth(path):
    """The growth saved at path, by model type, or {} when there is none."""
    if path is None:
        return {}
    try:
        with open(path) as f:
            saved = json.load(f)
        return {model_type: int(size) for model_type, size in saved.items()}
    except (OSError, ValueError, AttributeError, TypeError):
        return {}


def save_growth(path, growth):
    """
    Merge growth into what is saved at path, keeping the larger of the two
    for each model type, and swap the result into place whole.
    """
    if path is None:
        return
    merged = load_growth(path)
    for model_type, size in growth.items():
        merged[model_type] = max(size, merged.get(model_type, 0))
    partial = "%s.%d.tmp" % (path, os.getpid())
    try:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(partial, "w") as f:
            json.dump(merged, f, indent=2, sort_keys=True)
        os.replace(partial, path)
    except OSError:
        pass


def process_memory(pid):
    """
    (current, peak) resident set size of a process in bytes, or (0, 0)
    when unreadable.
    """
    current = peak = 0
    try:
        with open("/proc/%d/status" % pid) as f:
            for line in f:
                if line.startswith("VmRSS:"):
                    current = int(line.split()[1]) * 1024
                elif line.startswith("VmHWM:"):
                    peak = int(line.split()[1]) * 1024
    except (OSError, ValueError, IndexError):
        pass
    return current, peak


def reap(pid):
    """
    Wait for a worker to exit and return (exit code, peak resident set size
    in bytes).  The exit code is negative when a signal killed the worker,
    and the peak holds for a killed worker too.
    """
    _, status, usage = os.wait4(pid, 0)
    return os.waitstatus_to_exitcode(status), usage.ru_maxrss * 1024


def input_model_type(work_item):
    """
    The type of a work item's input model, which is what decides how much a
    generator loads (assembly, component, record, ...).  Work items are the
    tuples _pregenerate_codegen_targets builds, whose last entry is the input
    model filename.
    """
    input_filename = work_item[-1]
    parts = os.path.basename(input_filename).split(".")
    return parts[-2] if len(parts) >= 3 else parts[0]


class MemoryAwareScheduler:
    """
    Runs one worker per work item, admitting a new one only while the memory
    it is expected to need is available; the module docstring defines the
    terms.  The memory probes, the reaper, and the model type function are
    attributes so a test can script them.
    """

    PLACEHOLDER_EXPECTED_SIZE = 2 * GIB
    POLL_SECONDS = 0.25

    def __init__(self, target, max_workers=None, placeholder_expected_size=None, headroom=None, parent_size=None, model_type_of=input_model_type, store=None):
        """
        target is the function each worker runs on its work item, returning
        whether it succeeded.  The rest default to a worker per core, the
        placeholder expected size, the headroom for this host's memory, this
        process's resident size as the parent size, and the input model's
        type as the model type.  store is the file the measured growth is
        loaded from and saved to, or None to keep it in this scheduler only.
        """
        self.target = target
        self.model_type_of = model_type_of
        self.max_workers = max(1, multiprocessing.cpu_count() if max_workers is None else max_workers)
        self.placeholder_expected_size = self.PLACEHOLDER_EXPECTED_SIZE if placeholder_expected_size is None else placeholder_expected_size
        self.headroom = headroom_for(system_memory()[0]) if headroom is None else headroom
        self.parent_size = process_memory(os.getpid())[0] if parent_size is None else parent_size
        self.system_memory = system_memory
        self.process_memory = process_memory
        self.reap = reap
        self.store = store
        # The growth of each model type, and the model types that are measured.
        self.growth = load_growth(store)
        self.measured = set(self.growth)
        # Statistics for the caller's report: the most workers at once,
        # whether memory ever held one back, whether a probe ever did, and
        # how many the kernel killed.
        self.peak_workers = 0
        self.throttled = False
        self.probed = False
        self.killed = 0

    def expected_size(self, model_type):
        """
        Bytes a worker of this model type is expected to reach: the parent
        size plus the model type's growth, or the placeholder while the model
        type is unmeasured and has not grown past it.
        """
        expected = self.parent_size + self.growth.get(model_type, 0)
        if model_type in self.measured:
            return expected
        return max(self.placeholder_expected_size, expected)

    def _raise_growth(self, model_type, size):
        """Record a worker size seen for a model type when it is the largest growth so far."""
        growth = max(0, size - self.parent_size)
        if growth > self.growth.get(model_type, 0):
            self.growth[model_type] = growth

    def _observe(self, running):
        """
        Read each running worker's current and peak size and raise its model
        type's expected size to match.
        """
        for pid, item in running.values():
            self._raise_growth(self.model_type_of(item), max(self.process_memory(pid)))

    def _can_admit(self, running, item):
        """
        Whether the work item may start now.  An unmeasured model type runs
        one worker at a time, its probe.  With nothing running, one worker
        always starts.  Otherwise the free memory, less what the running workers are
        still expected to grow into, must cover this worker's expected size
        plus the headroom.
        """
        model_type = self.model_type_of(item)
        if model_type not in self.measured and any(self.model_type_of(other) == model_type for _, other in running.values()):
            self.probed = True
            return False
        if not running:
            return True
        _, available = self.system_memory()
        if available is None:
            return True
        # Memory the running workers are still expected to grow into:
        reserved = 0
        for pid, other in running.values():
            current, _ = self.process_memory(pid)
            reserved += max(0, self.expected_size(self.model_type_of(other)) - current)
        if available - reserved < self.expected_size(model_type) + self.headroom:
            self.throttled = True
            return False
        return True

    def _start(self, item):
        """
        Fork a worker for the work item and return (sentinel, pid).  The
        sentinel is the read end of a pipe whose only write end the worker
        holds, so it becomes readable when the worker exits, however it exits.
        """
        sentinel, exit_signal = os.pipe()
        sys.stdout.flush()
        sys.stderr.flush()
        pid = os.fork()
        if pid == 0:
            succeeded = False
            try:
                os.close(sentinel)
                succeeded = bool(self.target(item))
            finally:
                sys.stdout.flush()
                sys.stderr.flush()
                os._exit(0 if succeeded else 1)
        os.close(exit_signal)
        return sentinel, pid

    def run(self, work_items):
        """
        Run every work item in its own worker and return
        [(work_item, succeeded)] in completion order.  Each pass observes the
        running workers, admits from the front of the queue while allowed
        (an item waiting on its model type's probe is skipped over; one held
        back by memory ends the pass), waits for a worker to exit, and records
        its outcome and the peak size it reached.  A worker that exits by
        signal counts as failed.
        """
        pending = list(work_items)
        pending.reverse()
        running = {}  # sentinel -> (pid, work_item)
        finished = []
        while pending or running:
            self._observe(running)
            skipped = []
            while pending and len(running) < self.max_workers:
                item = pending.pop()
                if not self._can_admit(running, item):
                    skipped.append(item)
                    if self.model_type_of(item) in self.measured:
                        break  # Out of memory for now, not just waiting on a probe.
                    continue
                sentinel, pid = self._start(item)
                running[sentinel] = (pid, item)
                self.peak_workers = max(self.peak_workers, len(running))
            pending.extend(reversed(skipped))
            if not running:
                continue
            for sentinel in multiprocessing.connection.wait(list(running), timeout=self.POLL_SECONDS):
                pid, item = running.pop(sentinel)
                os.close(sentinel)
                exit_code, peak = self.reap(pid)
                if exit_code < 0:
                    self.killed += 1
                finished.append((item, exit_code == 0))
                model_type = self.model_type_of(item)
                self._raise_growth(model_type, peak)
                self.measured.add(model_type)
        save_growth(self.store, {model_type: self.growth.get(model_type, 0) for model_type in self.measured})
        return finished

    def report(self):
        """
        One line for the build output: the most workers at once, whether
        memory held any back, the parent size and each measured model type's
        growth, and how many workers the kernel killed.
        """
        learned = ", ".join(
            "%s +%d MiB" % (model_type, self.growth.get(model_type, 0) // MIB)
            for model_type in sorted(self.measured, key=lambda t: -self.growth.get(t, 0))
        )
        return "at most %d of %d workers (%s%d MiB parent; growth per worker: %s)%s" % (
            self.peak_workers,
            self.max_workers,
            "memory-limited; " if self.throttled else "",
            self.parent_size // MIB,
            learned or "unmeasured",
            (", %d killed by the kernel and left to redo" % self.killed) if self.killed else "",
        )


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
    # on-disk model_cache.db is safe for concurrent reads, occasional
    # cache-miss writes are serialized by filelock inside the DB layer.
    scheduler = MemoryAwareScheduler(_generate_one, store=growth_file())
    finished = scheduler.run(work_items)
    if scheduler.throttled or scheduler.probed or scheduler.killed:
        from util import redo
        redo.info_print("Pre-generated %d sources with %s" % (len(work_items), scheduler.report()))
    return [item[0] for item, ok in finished if ok]


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
