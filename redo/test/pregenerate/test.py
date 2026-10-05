#!/usr/bin/env python3

# Unit test for the memory-aware scheduler in redo/util/pregenerate.py.  It
# checks that every work item runs and reports its outcome, that the core
# count and free memory bound how many workers run at once, and that a worker
# the kernel kills fails its item without hanging the run.  It also checks
# how each model type's growth is probed, learned, saved, and read back, and
# how memory and headroom are read from the kernel and the cgroup.  Workers
# are real forked processes; the memory probes are scripted.
import multiprocessing
import os
import signal
import tempfile
import time

from util.pregenerate import GIB, MIB, MemoryAwareScheduler, growth_file, headroom_for, input_model_type, reap, system_memory

context = multiprocessing.get_context("fork")
live_workers = context.Value("i", 0)
peak_live_workers = context.Value("i", 0)


def work(item):
    _, seconds, outcome, _ = item
    with live_workers.get_lock():
        live_workers.value += 1
        peak_live_workers.value = max(peak_live_workers.value, live_workers.value)
    time.sleep(seconds)
    with live_workers.get_lock():
        live_workers.value -= 1
    if outcome == "kill":
        os.kill(os.getpid(), signal.SIGKILL)
    if outcome == "raise":
        raise RuntimeError("generator failed")
    return outcome == "ok"


def grow_then_kill(item):
    block = b"x" * (256 * MIB)
    os.kill(os.getpid(), signal.SIGKILL)
    return len(block)


def items(count, outcome="ok", seconds=0.15, model_type="a", prefix="item"):
    return [("%s_%d" % (prefix, i), seconds, outcome, model_type) for i in range(count)]


def scheduler(available=None, memory=(0, 0), measured=(), **kwargs):
    kwargs.setdefault("max_workers", 3)
    kwargs.setdefault("placeholder_expected_size", 1 * GIB)
    kwargs.setdefault("headroom", 512 * MIB)
    kwargs.setdefault("parent_size", 64 * MIB)
    kwargs.setdefault("model_type_of", lambda item: item[3])
    s = MemoryAwareScheduler(work, **kwargs)
    s.POLL_SECONDS = 0.02
    s.system_memory = lambda: (None, available)
    s.process_memory = lambda pid: memory
    s.reap = lambda pid: (reap(pid)[0], s.process_memory(pid)[1])
    for model_type, size in measured:
        s.measured.add(model_type)
        s.growth[model_type] = size - s.parent_size
    peak_live_workers.value = 0
    live_workers.value = 0
    return s


def run(s, work_items):
    started = time.time()
    finished = s.run(work_items)
    assert sorted(item[0] for item, _ in finished) == sorted(item[0] for item in work_items), "every item finishes exactly once"
    return dict((item[0], ok) for item, ok in finished), time.time() - started


# Memory unreadable, model type already measured: the core-count cap alone bounds the run.
s = scheduler(available=None, measured=[("a", 1 * GIB)])
results, _ = run(s, items(6))
assert all(results.values()), results
assert peak_live_workers.value == 3, peak_live_workers.value
assert s.peak_workers == 3 and not s.throttled and not s.probed and s.killed == 0

# Memory admission: 3 GiB available, 1 GiB expected per worker that has not
# grown yet, 512 MiB headroom, so a second worker fits and a third does not.
s = scheduler(available=3 * GIB, measured=[("a", 1 * GIB)])
results, _ = run(s, items(6))
assert all(results.values()), results
assert peak_live_workers.value == 2, peak_live_workers.value
assert s.peak_workers == 2 and s.throttled

# A worker that has already grown to its figure reserves nothing more, so
# the same memory admits a worker per core.
s = scheduler(available=3 * GIB, memory=(1 * GIB, 1 * GIB), measured=[("a", 1 * GIB)])
run(s, items(6))
assert peak_live_workers.value == 3, peak_live_workers.value
assert not s.throttled

# A starved host still progresses, one worker at a time.
s = scheduler(available=0, measured=[("a", 1 * GIB)])
results, _ = run(s, items(4))
assert all(results.values()), results
assert peak_live_workers.value == 1, peak_live_workers.value
assert s.throttled

# A generator failure and a kernel kill are both reported as failures, and
# the kill neither hangs nor stops the rest of the run.
s = scheduler(available=None, measured=[("a", 1 * GIB)])
work_items = items(2) + [("failed", 0.05, "fail", "a"), ("raised", 0.05, "raise", "a"), ("killed", 0.05, "kill", "a")] + items(2, prefix="later")
results, _ = run(s, work_items)
assert results["failed"] is False and results["raised"] is False and results["killed"] is False, results
assert all(ok for name, ok in results.items() if name not in ("failed", "raised", "killed")), results
assert s.killed == 1, s.killed

# A worker the kernel kills before any poll sees it still measures its model
# type at the size it reached, read from the reaped process itself.
s = scheduler(available=None)
s.reap = reap
s.target = grow_then_kill
results, _ = run(s, items(1, model_type="big"))
assert results["item_0"] is False and s.killed == 1, results
assert s.expected_size("big") >= 256 * MIB, s.expected_size("big")

# An unmeasured model type is probed one worker at a time: of three items the
# first runs alone and the other two together once it has finished.
s = scheduler(available=None)
_, elapsed = run(s, items(3, model_type="b"))
assert peak_live_workers.value == 2, peak_live_workers.value
assert "b" in s.measured and elapsed >= 0.3, elapsed
assert s.probed and not s.throttled

# Probes of different model types run side by side, and a model type waiting
# on its probe does not hold up the others behind it in the queue: both short
# "b" items finish while the long "a" probe ahead of them is still running.
s = scheduler(available=None)
work_items = items(2, model_type="a", prefix="a", seconds=0.4) + items(2, model_type="b", prefix="b", seconds=0.05)
results, _ = run(s, work_items)
order = list(results)
assert peak_live_workers.value == 2, peak_live_workers.value
assert order.index("a_0") > max(order.index("b_0"), order.index("b_1")), order

# A finished worker's growth past the parent sets its model type's expected
# size, below the placeholder when that is what it measured, and a small
# model type never lowers a big one.
s = scheduler(available=None, memory=(3 * GIB, 3 * GIB))
assert s.expected_size("a") == 1 * GIB, s.expected_size("a")
run(s, items(1, model_type="a"))
assert s.growth["a"] == 3 * GIB - 64 * MIB and s.expected_size("a") == 3 * GIB, s.growth
s.process_memory = lambda pid: (100 * MIB, 100 * MIB)
run(s, items(2, model_type="b"))
assert s.expected_size("b") == 100 * MIB and s.expected_size("a") == 3 * GIB, s.growth
assert s.expected_size("unseen") == 1 * GIB

# A running worker that outgrows its model type's expected size raises it before it finishes.
s = scheduler(available=None, memory=(5 * GIB, 5 * GIB), measured=[("a", 1 * GIB)])
run(s, items(1))
assert s.growth["a"] == 5 * GIB - 64 * MIB, s.growth
# ... and a model type that never grew past the parent is expected at the parent size.
s = scheduler(available=None, memory=(1, 1))
run(s, items(1, model_type="c"))
assert s.expected_size("c") == 64 * MIB, s.expected_size("c")

# A work item's model type is its input model's type.
assert input_model_type(("o", "m", "c", "f", "/p/my_assembly.assembly.yaml")) == "assembly"
assert input_model_type(("o", "m", "c", "f", "/p/my_component.component.yaml")) == "component"
assert input_model_type(("o", "m", "c", "f", "/p/my_assembly.ccsds_downsampler_filters.yaml")) == "ccsds_downsampler_filters"
assert input_model_type(("o", "m", "c", "f", "/p/odd.yaml")) == "odd"

# The cgroup's free memory leaves its inactive file cache out of the usage,
# for both hierarchies, and falls back to the raw usage without a
# memory.stat.  Total and available are each the lesser of the kernel's
# figure and the cgroup's.
with tempfile.TemporaryDirectory() as root:
    meminfo = os.path.join(root, "meminfo")
    with open(meminfo, "w") as f:
        f.write("MemTotal: %d kB\nMemAvailable: %d kB\n" % (64 * GIB // 1024, 32 * GIB // 1024))
    v2 = os.path.join(root, "v2")
    os.makedirs(v2)
    for name, content in (("memory.max", 8 * GIB), ("memory.current", 6 * GIB)):
        with open(os.path.join(v2, name), "w") as f:
            f.write("%d\n" % content)
    assert system_memory(meminfo, v2) == (8 * GIB, 2 * GIB), system_memory(meminfo, v2)
    with open(os.path.join(v2, "memory.stat"), "w") as f:
        f.write("anon %d\nactive_file 1\ninactive_file %d\n" % (1 * GIB, 5 * GIB))
    assert system_memory(meminfo, v2) == (8 * GIB, 7 * GIB), system_memory(meminfo, v2)
    v1 = os.path.join(root, "v1")
    os.makedirs(os.path.join(v1, "memory"))
    for name, content in (("memory.limit_in_bytes", "%d" % (8 * GIB)), ("memory.usage_in_bytes", "%d" % (6 * GIB)), ("memory.stat", "cache 1\ntotal_inactive_file %d" % (4 * GIB))):
        with open(os.path.join(v1, "memory", name), "w") as f:
            f.write(content + "\n")
    assert system_memory(meminfo, v1) == (8 * GIB, 6 * GIB), system_memory(meminfo, v1)
    with open(os.path.join(v2, "memory.max"), "w") as f:
        f.write("max\n")
    assert system_memory(meminfo, v2) == (64 * GIB, 32 * GIB), system_memory(meminfo, v2)
    assert system_memory(os.path.join(root, "absent"), os.path.join(root, "absent")) == (None, None)

# The headroom is a tenth of total memory, kept between 512 MiB and 2 GiB.
assert headroom_for(8 * GIB) == 8 * GIB // 10
assert headroom_for(2 * GIB) == 512 * MIB and headroom_for(64 * GIB) == 2 * GIB
assert headroom_for(None) == 512 * MIB

# The report gives the parent size and each measured model type's growth,
# largest first, and says memory-limited only when memory held a worker back.
report = s.report()
assert "at most 1 of 3 workers (64 MiB parent; growth per worker: c +0 MiB)" in report, report
s = scheduler(available=0, measured=[("a", 1 * GIB)])
run(s, items(2))
assert "memory-limited; 64 MiB parent; growth per worker: a +960 MiB" in s.report(), s.report()

# A scheduler with a store saves the growth it measured there, and a later
# one starts from it, expecting that growth on top of its own parent size.
# Saving keeps the larger growth of each model type already stored, and an
# unreadable store starts from nothing.
with tempfile.TemporaryDirectory() as root:
    store = os.path.join(root, "build", "growth.json")
    first = scheduler(available=None, memory=(2 * GIB, 2 * GIB), store=store)
    run(first, items(1, model_type="z"))
    second = scheduler(available=None, memory=(1 * GIB, 1 * GIB), parent_size=512 * MIB, store=store)
    assert "z" in second.measured and second.expected_size("z") == 512 * MIB + 2 * GIB - 64 * MIB, second.expected_size("z")
    run(second, items(1, model_type="y"))
    run(second, items(1, model_type="z"))
    third = scheduler(store=store)
    assert third.growth == {"z": 2 * GIB - 64 * MIB, "y": 512 * MIB}, third.growth
    with open(store, "w") as f:
        f.write("not json")
    assert scheduler(store=store).growth == {}

# The store is the build directory of the directory the top-level redo ran in.
os.environ["REDO_INIT_PATH"] = "/top"
assert growth_file() == "/top/build/pregenerate_worker_growth.json", growth_file()
del os.environ["REDO_INIT_PATH"]
assert growth_file() is None

# The defaults are a worker per core, the placeholder expected size, the
# headroom for this host, this process's own size as the parent size, and
# no store.
s = MemoryAwareScheduler(work)
assert s.max_workers == max(1, multiprocessing.cpu_count()), s.max_workers
assert s.placeholder_expected_size == MemoryAwareScheduler.PLACEHOLDER_EXPECTED_SIZE
assert s.headroom == headroom_for(system_memory()[0]), s.headroom
assert s.parent_size > 0 and s.store is None and s.growth == {}

print("All pregenerate scheduler tests passed.")
