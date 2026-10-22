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
workers, must cover one more worker plus a headroom reserve.  A worker that
exits by signal counts as failed, and its target goes through redo-ifchange
like any other failed target.  Where memory cannot be read, the core count
alone bounds the workers.

Terms used throughout:

  worker         a forked process generating one target.
  kind           the type of a target's input model (assembly, component,
                 record, ...), which is what sets how much its generator
                 loads.  Expected sizes are kept per kind.
  expected size  the bytes a worker of a kind is expected to reach: the
                 largest size any worker of the kind has been seen at,
                 starting from a placeholder until one has finished, and
                 never below a floor.
  measured       a kind one worker has finished for, so its expected size
                 is a measurement.
  probe          the one worker of an unmeasured kind allowed to run at a
                 time, which measures the kind for the rest.

The expected sizes persist for the life of the process, so each batch of a
build step starts from what the earlier batches measured.

Setting DISABLE_PREGEN in the environment skips pre-generation entirely and
every source goes through redo-ifchange.
"""
import io
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

        from util import redo
        redo.redo_done(source, all_deps)

        return source

    except Exception:
        # Generation failed. Clean up any partial output and let
        # redo handle this target through its normal .do script path.
        try:
            if os.path.exists(source):
                os.remove(source)
        except Exception:
            pass
        return None


def _worker_main(work_item):
    """Worker entry point: generates one work item and exits 0 on success, 1 on failure."""
    sys.exit(0 if _generate_one(work_item) else 1)


def available_memory():
    """
    Bytes a new worker could use: the lesser of what the kernel reports
    available and what the cgroup still allows.  None when neither is
    readable (not Linux, or no /proc), in which case the scheduler falls
    back to the core count alone.
    """
    def read_meminfo_available():
        """Bytes the kernel reports as available, or None when unreadable."""
        try:
            with open("/proc/meminfo") as f:
                for line in f:
                    if line.startswith("MemAvailable:"):
                        return int(line.split()[1]) * 1024
        except (OSError, ValueError, IndexError):
            pass
        return None

    def read_cgroup_available():
        """
        Bytes left under the cgroup memory limit, or None when there is no
        limit or it cannot be read.  Checks the unified (v2) hierarchy first,
        then the legacy (v1) memory controller, which reports no limit as a
        huge number near the page-aligned maximum.
        """
        for limit_path, usage_path in (
            ("/sys/fs/cgroup/memory.max", "/sys/fs/cgroup/memory.current"),
            ("/sys/fs/cgroup/memory/memory.limit_in_bytes", "/sys/fs/cgroup/memory/memory.usage_in_bytes"),
        ):
            try:
                with open(limit_path) as f:
                    limit = f.read().strip()
                with open(usage_path) as f:
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
            return max(0, limit - usage)
        return None

    readings = [r for r in (read_meminfo_available(), read_cgroup_available()) if r is not None]
    return min(readings) if readings else None


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


def kind_of_work_item(work_item):
    """
    The kind of target a work item is: the type of its input model, which
    is what decides how much a generator loads (assembly, component,
    record, ...).  Work items are the tuples _pregenerate_codegen_targets
    builds, whose last entry is the input model filename.
    """
    input_filename = work_item[-1]
    parts = os.path.basename(input_filename).split(".")
    return parts[-2] if len(parts) >= 3 else parts[0]


# The expected sizes learned so far in this process, by kind, and the kinds
# that are measured; shared by every scheduler the process creates.
_learned_expected_sizes = {}
_measured_kinds = set()


class MemoryAwareScheduler:
    """
    Runs one worker per work item, admitting a new one only while the memory
    it is expected to need is available; the module docstring defines the
    terms.  The memory probes and the kind function are attributes so a test
    can script them, and a test passes its own expected sizes and measured
    set to start from nothing.
    """

    # The floor under every expected size, the expected size of a kind no
    # worker has finished for, the memory kept free for everything else, and
    # how long to wait for a worker to exit before looking again.
    MINIMUM_EXPECTED_SIZE = 64 * 1024 * 1024
    PLACEHOLDER_EXPECTED_SIZE = 2048 * 1024 * 1024
    HEADROOM = 1024 * 1024 * 1024
    POLL_SECONDS = 0.25

    def __init__(self, target, max_workers=None, worker_estimate=None, headroom=None, kind_of=kind_of_work_item, expected_sizes=None, measured=None):
        """
        target is the function each worker runs on its work item.  The rest
        default to a worker per core, the placeholder expected size and the
        headroom in MiB, the input model's type as the kind, and the process's
        shared expected sizes and measured kinds.
        """
        self.target = target
        self.kind_of = kind_of
        self.max_workers = max(1, multiprocessing.cpu_count() if max_workers is None else max_workers)
        self.estimate = max(self.MINIMUM_EXPECTED_SIZE, self.PLACEHOLDER_EXPECTED_SIZE if worker_estimate is None else worker_estimate * 1024 * 1024)
        self.headroom = self.HEADROOM if headroom is None else headroom * 1024 * 1024
        self.available_memory = available_memory
        self.process_memory = process_memory
        self.context = multiprocessing.get_context("fork")
        # The expected size of each kind, and the kinds that are measured.
        self.expected_sizes = _learned_expected_sizes if expected_sizes is None else expected_sizes
        self.measured = _measured_kinds if measured is None else measured
        # Statistics for the caller's report: the most workers at once,
        # whether memory ever held one back, whether a probe ever did, and
        # how many the kernel killed.
        self.peak_workers = 0
        self.throttled = False
        self.probed = False
        self.killed = 0

    def expected_size(self, kind):
        """
        Bytes a worker of this kind is expected to reach: the largest seen for
        the kind, or the placeholder while the kind is unmeasured, never below
        the floor.
        """
        learned = self.expected_sizes.get(kind, 0)
        if kind in self.measured:
            return max(self.MINIMUM_EXPECTED_SIZE, learned)
        return max(self.estimate, learned)

    def _raise_expected_size(self, kind, size):
        """Record a size seen for a kind when it is the largest so far."""
        if size > self.expected_sizes.get(kind, 0):
            self.expected_sizes[kind] = size

    def _observe(self, running):
        """
        Read each running worker's current and peak size, keep the largest
        seen for it, and raise its kind's expected size to match.
        """
        for entry in running.values():
            proc, item, _ = entry
            current, peak = self.process_memory(proc.pid)
            entry[2] = max(entry[2], current, peak)
            self._raise_expected_size(self.kind_of(item), entry[2])

    def _can_admit(self, running, item):
        """
        Whether the work item may start now.  An unmeasured kind runs one
        worker at a time, its probe.  With nothing running, one worker always
        starts.  Otherwise the free memory, less what the running workers are
        still expected to grow into, must cover this worker's expected size
        plus the headroom.
        """
        kind = self.kind_of(item)
        if kind not in self.measured and any(self.kind_of(other) == kind for _, other, _ in running.values()):
            self.probed = True
            return False
        if not running:
            return True
        available = self.available_memory()
        if available is None:
            return True
        # Memory the running workers are still expected to grow into:
        reserved = 0
        for proc, other, seen in running.values():
            current, _ = self.process_memory(proc.pid)
            reserved += max(0, self.expected_size(self.kind_of(other)) - current)
        if available - reserved < self.expected_size(kind) + self.headroom:
            self.throttled = True
            return False
        return True

    def run(self, work_items):
        """
        Run every work item in its own worker and return
        [(work_item, succeeded)] in completion order.  Each pass observes the
        running workers, admits from the front of the queue while allowed
        (an item waiting on its kind's probe is skipped over; one held back
        by memory ends the pass), waits for a worker to exit, and records its
        outcome and the largest size it reached.  A worker that exits by
        signal counts as failed.
        """
        pending = list(work_items)
        pending.reverse()
        running = {}  # sentinel -> [process, work_item, largest size seen]
        finished = []
        while pending or running:
            self._observe(running)
            skipped = []
            while pending and len(running) < self.max_workers:
                item = pending.pop()
                if not self._can_admit(running, item):
                    skipped.append(item)
                    if self.kind_of(item) in self.measured:
                        break  # Out of memory for now, not just waiting on a probe.
                    continue
                proc = self.context.Process(target=self.target, args=(item,))
                proc.start()
                running[proc.sentinel] = [proc, item, 0]
                self.peak_workers = max(self.peak_workers, len(running))
            pending.extend(reversed(skipped))
            if not running:
                continue
            for sentinel in multiprocessing.connection.wait(list(running), timeout=self.POLL_SECONDS):
                proc, item, seen = running.pop(sentinel)
                _, peak = self.process_memory(proc.pid)
                proc.join()
                if proc.exitcode is not None and proc.exitcode < 0:
                    self.killed += 1
                finished.append((item, proc.exitcode == 0))
                kind = self.kind_of(item)
                self._raise_expected_size(kind, max(seen, peak))
                self.measured.add(kind)
        return finished

    def report(self):
        """
        One line for the build output: the most workers at once, whether
        memory held any back, each measured kind's expected size, and how
        many workers the kernel killed.
        """
        learned = ", ".join(
            "%s %d MiB" % (kind, self.expected_size(kind) // (1024 * 1024))
            for kind in sorted(self.measured, key=lambda k: -self.expected_size(k))
        )
        return "at most %d of %d workers (%sper worker: %s)%s" % (
            self.peak_workers,
            self.max_workers,
            "memory-limited; " if self.throttled else "",
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
    scheduler = MemoryAwareScheduler(_worker_main)
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
