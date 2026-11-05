#!/usr/bin/env python3

# Unit test for the memory-aware scheduler in redo/util/pregenerate.py: that
# it runs every item and reports each outcome, that the core-count cap and
# the memory admission rule bound how many workers run at once, that a
# starved host still makes progress one worker at a time, that a worker the
# kernel kills is reported as failed instead of hanging the run, that a kind
# of target is probed one worker at a time until one has finished, that the
# per-kind figure is learned from finished and running workers without one
# kind lowering another's, that the kind of a work item is its input model
# type, and that the defaults are a worker per core and the placeholder figure. The work is a small
# function run in real forked processes with the memory probes scripted.
import multiprocessing
import os
import signal
import sys
import time

from util.pregenerate import MemoryAwareScheduler, kind_of_work_item

MIB = 1024 * 1024
GIB = 1024 * MIB

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
    sys.exit(0 if outcome == "ok" else 1)


def items(count, outcome="ok", seconds=0.15, kind="a", prefix="item"):
    return [("%s_%d" % (prefix, i), seconds, outcome, kind) for i in range(count)]


def scheduler(available=None, memory=(0, 0), measured=(), **kwargs):
    kwargs.setdefault("max_workers", 3)
    kwargs.setdefault("worker_estimate", 1024)
    kwargs.setdefault("headroom", 512)
    kwargs.setdefault("kind_of", lambda item: item[3])
    kwargs.setdefault("expected_sizes", {})
    kwargs.setdefault("measured", set())
    s = MemoryAwareScheduler(work, **kwargs)
    s.POLL_SECONDS = 0.02
    s.available_memory = lambda: available
    s.process_memory = lambda pid: memory
    for kind, size in measured:
        s.measured.add(kind)
        s.expected_sizes[kind] = size
    peak_live_workers.value = 0
    live_workers.value = 0
    return s


def run(s, work_items):
    started = time.time()
    finished = s.run(work_items)
    assert sorted(item[0] for item, _ in finished) == sorted(item[0] for item in work_items), "every item finishes exactly once"
    return dict((item[0], ok) for item, ok in finished), time.time() - started


# Memory unreadable, kind already measured: the core-count cap alone bounds the run.
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
work_items = items(2) + [("failed", 0.05, "fail", "a"), ("killed", 0.05, "kill", "a")] + items(2, prefix="later")
results, _ = run(s, work_items)
assert results["failed"] is False and results["killed"] is False, results
assert all(ok for name, ok in results.items() if name not in ("failed", "killed")), results
assert s.killed == 1, s.killed

# An unmeasured kind is probed one worker at a time: of three items the
# first runs alone and the other two together once it has finished.
s = scheduler(available=None)
_, elapsed = run(s, items(3, kind="b"))
assert peak_live_workers.value == 2, peak_live_workers.value
assert "b" in s.measured and elapsed >= 0.3, elapsed
assert s.probed and not s.throttled

# Probes of different kinds run side by side, and a kind waiting on its probe
# does not hold up the others behind it in the queue.
s = scheduler(available=None)
work_items = items(2, kind="a", prefix="a") + items(2, kind="b", prefix="b")
_, elapsed = run(s, work_items)
assert peak_live_workers.value == 2, peak_live_workers.value
assert elapsed < 0.5, elapsed

# A finished worker's peak becomes its kind's figure, below the estimate when
# that is what it measured, and a small kind never lowers a big one.
s = scheduler(available=None, memory=(3 * GIB, 3 * GIB))
assert s.expected_size("a") == 1 * GIB, s.expected_size("a")
run(s, items(1, kind="a"))
assert s.expected_sizes["a"] == 3 * GIB and s.expected_size("a") == 3 * GIB, s.expected_sizes
s.process_memory = lambda pid: (100 * MIB, 100 * MIB)
run(s, items(2, kind="b"))
assert s.expected_size("b") == 100 * MIB and s.expected_size("a") == 3 * GIB, s.expected_sizes
assert s.expected_size("unseen") == 1 * GIB

# A running worker that outgrows its kind's figure raises it before it finishes.
s = scheduler(available=None, memory=(5 * GIB, 5 * GIB), measured=[("a", 1 * GIB)])
run(s, items(1))
assert s.expected_sizes["a"] == 5 * GIB, s.expected_sizes
# ... and the floor holds for a kind that measured tiny.
s = scheduler(available=None, memory=(1, 1))
run(s, items(1, kind="c"))
assert s.expected_size("c") == MemoryAwareScheduler.MINIMUM_EXPECTED_SIZE, s.expected_size("c")

# The kind of a work item is its input model's type.
assert kind_of_work_item(("o", "m", "c", "f", "/p/flight.assembly.yaml")) == "assembly"
assert kind_of_work_item(("o", "m", "c", "f", "/p/das_commander.component.yaml")) == "component"
assert kind_of_work_item(("o", "m", "c", "f", "/p/sbc.ccsds_downsampler_filters.yaml")) == "ccsds_downsampler_filters"
assert kind_of_work_item(("o", "m", "c", "f", "/p/odd.yaml")) == "odd"

# The report names the measured kinds, largest first, and says memory-limited
# only when memory held a worker back.
report = s.report()
assert "at most 1 of 3 workers (per worker: c 64 MiB)" in report, report
s = scheduler(available=0, measured=[("a", 1 * GIB)])
run(s, items(2))
assert "memory-limited; per worker: a 1024 MiB" in s.report(), s.report()

# Without its own figures a scheduler shares what earlier ones in the
# process learned, so a later batch starts informed.
first = scheduler(available=None, memory=(2 * GIB, 2 * GIB))
first.expected_sizes, first.measured = {}, set()
shared_expected_sizes, shared_measured = first.expected_sizes, first.measured
run(first, items(1, kind="z"))
second = MemoryAwareScheduler(work, kind_of=lambda item: item[3], expected_sizes=shared_expected_sizes, measured=shared_measured)
assert second.expected_size("z") == 2 * GIB and "z" in second.measured
default_one = MemoryAwareScheduler(work)
default_two = MemoryAwareScheduler(work)
assert default_one.expected_sizes is default_two.expected_sizes and default_one.measured is default_two.measured

# The defaults are a worker per core, the placeholder figure, and the headroom.
s = MemoryAwareScheduler(work)
assert s.max_workers == max(1, multiprocessing.cpu_count()), s.max_workers
assert s.estimate == MemoryAwareScheduler.PLACEHOLDER_EXPECTED_SIZE and s.headroom == MemoryAwareScheduler.HEADROOM

print("All pregenerate scheduler tests passed.")
