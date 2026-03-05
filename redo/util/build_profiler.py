"""
Build system profiler — collects timing data for build phases.
Add/remove this file independently of other changes.

Usage:
    from util.build_profiler import profiler
    profiler.start("phase_name")
    ...
    profiler.stop("phase_name")

    # At end of build:
    profiler.report()
"""
import time
import os
import json
import atexit

_PROFILE_FILE = os.environ.get("BUILD_PROFILE_FILE", "/tmp/adamant_build_profile.jsonl")


class BuildProfiler:
    def __init__(self):
        self._timers = {}  # name -> start_time
        self._records = []  # list of {name, start, end, duration, pid}
        self._enabled = os.environ.get("BUILD_PROFILE", "0") == "1"
        if self._enabled:
            atexit.register(self._flush)

    def start(self, name):
        if not self._enabled:
            return
        self._timers[name] = time.monotonic()

    def stop(self, name):
        if not self._enabled:
            return
        end = time.monotonic()
        start = self._timers.pop(name, None)
        if start is not None:
            record = {
                "name": name,
                "start": start,
                "duration": end - start,
                "pid": os.getpid(),
                "wall_time": time.time(),
            }
            self._records.append(record)
            self._write_record(record)

    def _write_record(self, record):
        try:
            with open(_PROFILE_FILE, "a") as f:
                f.write(json.dumps(record) + "\n")
        except Exception:
            pass

    def _flush(self):
        # Write any still-running timers as incomplete
        now = time.monotonic()
        for name, start in self._timers.items():
            record = {
                "name": name,
                "start": start,
                "duration": now - start,
                "pid": os.getpid(),
                "wall_time": time.time(),
                "incomplete": True,
            }
            self._write_record(record)

    def report(self):
        if not self._records:
            return ""
        lines = ["=== Build Profile Report ==="]
        # Group by name and sum durations
        totals = {}
        counts = {}
        for r in self._records:
            name = r["name"]
            totals[name] = totals.get(name, 0) + r["duration"]
            counts[name] = counts.get(name, 0) + 1
        for name in sorted(totals, key=totals.get, reverse=True):
            lines.append(f"  {name}: {totals[name]:.3f}s ({counts[name]} calls)")
        return "\n".join(lines)


profiler = BuildProfiler()
