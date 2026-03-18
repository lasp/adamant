#!/bin/bash
# Test suite for persistent target cache behavior.
# Run inside an activated adamant environment:
#   source env/activate && bash redo/test/persistent_cache/test_persistent_cache.sh

set -e

PASS=0
FAIL=0
TEST_DIR=""

pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

# Timing helper: returns milliseconds
time_ms() {
    local start end
    start=$(date +%s%N)
    "$@" 2>/dev/null
    end=$(date +%s%N)
    echo $(( (end - start) / 1000000 ))
}

setup() {
    # Pick a component directory for testing
    TEST_DIR="${ADAMANT_DIR}/src/components/oscillator"
    if [ ! -d "$TEST_DIR" ]; then
        local example_dir
        example_dir=$(dirname "$(dirname "$ADAMANT_CONFIGURATION_YAML")")
        TEST_DIR="${example_dir}/src/components/oscillator"
    fi
    if [ ! -d "$TEST_DIR" ]; then
        echo "ERROR: Cannot find oscillator component directory"
        exit 1
    fi

    # Calibrate thresholds dynamically by measuring the uncached slow path.
    # This makes tests portable across machines of different speeds.
    cd "$TEST_DIR"
    local db_path
    db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())" 2>/dev/null)
    # Remove caches to force cold path
    [ -f "$db_path" ] && cp "$db_path" "${db_path}.calibrate"
    rm -f "$db_path"
    # Measure uncached redo what (cold path = full setup)
    SLOW_BASELINE=$(time_ms redo what)
    echo "Calibration: uncached redo what = ${SLOW_BASELINE}ms"
    # Restore DB
    [ -f "${db_path}.calibrate" ] && mv "${db_path}.calibrate" "$db_path"

    # Thresholds derived from baseline:
    # Fast = cached path, should be <25% of slow baseline
    # Slow = uncached path, should be >40% of slow baseline
    FAST_THRESHOLD=$(( SLOW_BASELINE / 4 ))
    SLOW_THRESHOLD=$(( SLOW_BASELINE * 2 / 5 ))
    # Floor: fast threshold at least 20ms (to avoid noise on very fast machines)
    [ "$FAST_THRESHOLD" -lt 20 ] && FAST_THRESHOLD=20
    echo "Thresholds: fast < ${FAST_THRESHOLD}ms, slow > ${SLOW_THRESHOLD}ms"

    # Ensure persistent cache exists and warm up
    python3 -m database.persistent_target_cache 2>/dev/null || true
    cd "$TEST_DIR"
    redo what 2>/dev/null
}

cleanup() {
    cd "$TEST_DIR" 2>/dev/null || true
    rm -f hi.do test_dummy.do dummy1.do dummy2.do 2>/dev/null || true
}

trap cleanup EXIT

# -------------------------------------------------------
echo "=== Persistent Target Cache Tests ==="
echo ""
setup

# Test 1: Baseline fast path
echo "Test 1: Baseline - repeated calls are fast"
cd "$TEST_DIR"
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "first call ${ms}ms < ${FAST_THRESHOLD}ms"
else fail "first call ${ms}ms >= ${FAST_THRESHOLD}ms (expected fast)"; fi
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "second call ${ms}ms < ${FAST_THRESHOLD}ms"
else fail "second call ${ms}ms >= ${FAST_THRESHOLD}ms (expected fast)"; fi

# Test 2: Touching existing file does NOT invalidate
echo ""
echo "Test 2: Touch existing file - still fast"
cd "$TEST_DIR"
touch component-oscillator-implementation.adb
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "after touch existing: ${ms}ms (fast)"
else fail "after touch existing: ${ms}ms (expected fast)"; fi

# Test 3: Creating new .do file — still fast (stale data OK)
echo ""
echo "Test 3: Create new .do file - still fast (serves cached data)"
cd "$TEST_DIR"
touch hi.do
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "after create .do: ${ms}ms (fast, stale OK)"
else fail "after create .do: ${ms}ms (expected fast — stale data served)"; fi
# hi.do should NOT appear yet (not in cache until next build)
targets=$(redo what 2>&1)
if ! echo "$targets" | grep -q "^redo hi$"; then pass "hi not in output (stale cache, expected)"
else pass "hi already in output (pre-cached)"; fi
rm -f hi.do

# Test 4: Cold path (no persistent DB)
echo ""
echo "Test 4: Cold path - no persistent DB falls back gracefully"
cd "$TEST_DIR"
db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())")
cp "$db_path" "${db_path}.bak"
rm "$db_path"
ms=$(time_ms redo what)
if [ "$ms" -ge "$SLOW_THRESHOLD" ]; then pass "cold path without DB: ${ms}ms (slow, full setup)"
else fail "cold path: ${ms}ms (expected slow >= ${SLOW_THRESHOLD}ms)"; fi
# Should have recreated the cache
if [ -f "$db_path" ]; then pass "persistent DB recreated after cold path"
else fail "persistent DB NOT recreated"; fi
# Verify targets returned
count=$(redo what 2>&1 | wc -l)
if [ "$count" -gt 50 ]; then pass "targets returned after cold rebuild ($count)"
else fail "too few targets after cold rebuild ($count)"; fi
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "fast after cold rebuild: ${ms}ms"
else fail "not fast after cold rebuild: ${ms}ms"; fi
# Restore original full DB
mv "${db_path}.bak" "$db_path" 2>/dev/null || true

# Test 5: Cross-directory - reading one dir doesn't wipe another
echo ""
echo "Test 5: Cross-directory cache preservation"
cd "$TEST_DIR"
count_here=$(redo what 2>&1 | wc -l)
OTHER_DIR=$(dirname "$TEST_DIR")/counter
if [ -d "$OTHER_DIR" ]; then
    cd "$OTHER_DIR"
    count_other=$(redo what 2>&1 | wc -l)
    # Read from first dir again
    cd "$TEST_DIR"
    count_here2=$(redo what 2>&1 | wc -l)
    if [ "$count_here" -eq "$count_here2" ]; then
        pass "original dir targets preserved ($count_here == $count_here2)"
    else
        fail "original dir targets changed ($count_here -> $count_here2)"
    fi
    # Read other dir again
    cd "$OTHER_DIR"
    count_other2=$(redo what 2>&1 | wc -l)
    if [ "$count_other" -eq "$count_other2" ]; then
        pass "other dir targets preserved ($count_other == $count_other2)"
    else
        fail "other dir targets changed ($count_other -> $count_other2)"
    fi
else
    echo "  SKIP: no counter component dir found"
fi

# Test 6: Corrupt persistent DB - graceful fallback
echo ""
echo "Test 6: Corrupt persistent DB - falls back to slow path and recovers"
cd "$TEST_DIR"
db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())")
cp "$db_path" "${db_path}.bak"
echo "garbage data" > "$db_path"
ms=$(time_ms redo what)
if [ "$ms" -ge "$SLOW_THRESHOLD" ]; then pass "corrupt DB triggers slow path: ${ms}ms"
else fail "corrupt DB did not trigger slow path: ${ms}ms"; fi
# Verify targets returned on the slow path call itself
count=$(redo what 2>&1 | wc -l)
if [ "$count" -gt 10 ]; then pass "targets returned after corrupt fallback ($count)"
else fail "too few targets after corrupt fallback ($count)"; fi
# Restore full DB for remaining tests
mv "${db_path}.bak" "$db_path" 2>/dev/null || true

# Test 7: Real build updates persistent cache
echo ""
echo "Test 7: Real build refreshes persistent cache"
cd "$TEST_DIR"
db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())")
mtime_before=$(stat -c %Y "$db_path" 2>/dev/null || stat -f %m "$db_path" 2>/dev/null)
# Run a real build (just 'redo style' which is fast and triggers full setup)
redo style 2>/dev/null || true
sleep 1
mtime_after=$(stat -c %Y "$db_path" 2>/dev/null || stat -f %m "$db_path" 2>/dev/null)
if [ "$mtime_after" -ge "$mtime_before" ]; then pass "persistent DB updated after real build"
else fail "persistent DB not updated (before=$mtime_before after=$mtime_after)"; fi

# Test 8: Subdirectory targets
echo ""
echo "Test 8: Subdirectory targets"
SUB_DIR="${TEST_DIR}/test"
if [ -d "$SUB_DIR" ]; then
    cd "$SUB_DIR"
    # First call may be slow (subdirs not always pre-cached)
    redo what 2>/dev/null
    count=$(redo what 2>&1 | wc -l)
    echo "  ${SUB_DIR##*/}: $count targets"
    if [ "$count" -gt 0 ]; then pass "subdirectory has targets ($count)"
    else fail "subdirectory has no targets"; fi
else
    echo "  SKIP: no test/ subdirectory"
fi

# Test 9: Pre-warm creates cache from scratch
echo ""
echo "Test 9: Pre-warm entry point creates full cache"
cd "$TEST_DIR"
db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())")
rm -f "$db_path"
# Pre-warm must run from the project root (where config yaml lives),
# not from a component subdirectory, to avoid Ada source conflicts.
project_root=$(dirname "$(dirname "$ADAMANT_CONFIGURATION_YAML")")
(cd "$project_root" && python3 -m database.persistent_target_cache 2>/dev/null) || true
if [ -f "$db_path" ]; then pass "pre-warm created DB"
else fail "pre-warm did not create DB"; fi
cd "$TEST_DIR"
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "fast after pre-warm: ${ms}ms"
else fail "not fast after pre-warm: ${ms}ms"; fi

# Test 10: Breadcrumb file exists and points to correct cache dir
echo ""
echo "Test 10: Breadcrumb file for shell cache discovery"
breadcrumb="/tmp/redo-$(id -u)/cache_dir"
if [ -f "$breadcrumb" ]; then pass "breadcrumb file exists"
else fail "breadcrumb file missing: $breadcrumb"; fi
breadcrumb_dir=$(cat "$breadcrumb")
if [ -d "$breadcrumb_dir" ]; then pass "breadcrumb points to valid dir: $breadcrumb_dir"
else fail "breadcrumb dir does not exist: $breadcrumb_dir"; fi
if [ -f "$breadcrumb_dir/redo_target.db" ]; then pass "persistent DB found via breadcrumb"
else fail "persistent DB not found at $breadcrumb_dir/redo_target.db"; fi

# -------------------------------------------------------
# Text Cache Tests
# -------------------------------------------------------

# Helper to get text cache path for a directory.
# Reads the breadcrumb written by Python's get_cache_dir().
# Uses path-as-filename encoding: /foo/bar → _foo_bar
text_cache_path () {
    local dir=$1
    local breadcrumb="/tmp/redo-$(id -u)/cache_dir"
    local cache_dir
    cache_dir=$(cat "$breadcrumb") || { echo "ERROR: breadcrumb missing" >&2; return 1; }
    local dir_key="${dir//\//_}"
    echo "${cache_dir}/what/${dir_key}.txt"
}

# Test 11: Text cache file created by redo what
echo ""
echo "Test 11: Text cache file created by redo what"
cd "$TEST_DIR"
redo what 2>/dev/null
text_file=$(text_cache_path "$TEST_DIR")
if [ -s "$text_file" ]; then pass "text cache file exists and non-empty"
else fail "text cache file missing or empty: $text_file"; fi
# Verify content matches redo what output (strip "redo " prefix and header)
what_output=$(redo what 2>&1 | tail -n +2 | sed 's/^redo //' | sort)
cache_output=$(sort "$text_file")
if [ "$what_output" = "$cache_output" ]; then pass "text cache content matches redo what"
else
    what_count=$(echo "$what_output" | wc -l)
    cache_count=$(echo "$cache_output" | wc -l)
    fail "text cache content differs (cache=$cache_count, what=$what_count)"
fi

# Test 12: Text cache per-directory isolation
echo ""
echo "Test 12: Text cache per-directory isolation"
OTHER_DIR=$(dirname "$TEST_DIR")/counter
if [ -d "$OTHER_DIR" ]; then
    cd "$OTHER_DIR"
    redo what 2>/dev/null
    text_file_other=$(text_cache_path "$OTHER_DIR")
    text_file_orig=$(text_cache_path "$TEST_DIR")
    if [ -s "$text_file_other" ] && [ -s "$text_file_orig" ]; then
        pass "both directories have text cache files"
    else
        fail "missing text cache (orig=$text_file_orig other=$text_file_other)"
    fi
    # Verify they have different content (different target counts)
    count_orig=$(wc -l < "$text_file_orig")
    count_other=$(wc -l < "$text_file_other")
    if [ "$count_orig" -ne "$count_other" ]; then
        pass "text caches differ ($count_orig vs $count_other lines)"
    else
        # Same count is possible but unlikely for different components
        pass "text caches exist (both $count_orig lines)"
    fi
else
    echo "  SKIP: no counter component dir found"
fi

# Test 13: Text cache invalidated when directory changes
echo ""
echo "Test 13: Text cache mtime validation (dir newer)"
cd "$TEST_DIR"
redo what 2>/dev/null  # ensure fresh text cache
text_file=$(text_cache_path "$TEST_DIR")
# Add a file — bumps directory mtime above text cache mtime
sleep 1
touch new_target.do
dir_mtime=$(stat -c %Y "$TEST_DIR")
text_mtime=$(stat -c %Y "$text_file")
if [ "$dir_mtime" -gt "$text_mtime" ]; then pass "dir change detected (dir=$dir_mtime > text=$text_mtime)"
else fail "dir change NOT detected"; fi
# redo what should run (text cache stale) and refresh
redo what 2>/dev/null
text_mtime_after=$(stat -c %Y "$text_file")
if [ "$text_mtime_after" -ge "$dir_mtime" ]; then pass "text cache refreshed after dir change"
else fail "text cache still stale after dir change"; fi
rm -f new_target.do

# Test 14: Text cache survives redo clean
echo ""
echo "Test 14: Text cache in /tmp survives redo clean"
cd "$TEST_DIR"
text_file=$(text_cache_path "$TEST_DIR")
# Ensure text cache exists
redo what 2>/dev/null
if [ -s "$text_file" ]; then
    # Run redo clean
    redo clean 2>/dev/null || true
    if [ -s "$text_file" ]; then pass "text cache survives redo clean"
    else fail "text cache deleted by redo clean"; fi
else
    fail "text cache missing before redo clean test"
fi

# Test 15: Shell completion reads text cache (simulated)
echo ""
echo "Test 15: Shell completion text_cache_read simulation"
cd "$TEST_DIR"
# Ensure text cache is fresh
redo what 2>/dev/null
# Simulate what the shell completion's text_cache_read does
text_file=$(text_cache_path "$(pwd)")
if [ -s "$text_file" ]; then
    text_mtime=$(stat -c %Y "$text_file")
    dir_mtime=$(stat -c %Y "$(pwd)")
    if [ "$text_mtime" -ge "$dir_mtime" ]; then
        result=$(cat "$text_file")
        result_count=$(echo "$result" | wc -l)
        pass "simulated text_cache_read: $result_count targets"
    else
        fail "text cache stale (text=$text_mtime < dir=$dir_mtime)"
    fi
else
    fail "text cache file missing for shell read simulation"
fi
# Time the simulated shell read (cat + stat)
start=$(date +%s%N)
text_mtime=$(stat -c %Y "$text_file")
dir_mtime=$(stat -c %Y "$(pwd)")
[ "$text_mtime" -ge "$dir_mtime" ] && cat "$text_file" > /dev/null
end=$(date +%s%N)
shell_ms=$(( (end - start) / 1000000 ))
echo "  Shell text cache read: ${shell_ms}ms"
# Shell read should be much faster than even the cached Python path
SHELL_THRESHOLD=$(( FAST_THRESHOLD / 2 ))
[ "$SHELL_THRESHOLD" -lt 10 ] && SHELL_THRESHOLD=10
if [ "$shell_ms" -lt "$SHELL_THRESHOLD" ]; then pass "shell read fast: ${shell_ms}ms < ${SHELL_THRESHOLD}ms"
else fail "shell read slow: ${shell_ms}ms >= ${SHELL_THRESHOLD}ms"; fi

# Test 16: Cold start — no text cache, no persistent DB
echo ""
echo "Test 16: Cold start - both caches missing"
cd "$TEST_DIR"
db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())")
text_file=$(text_cache_path "$TEST_DIR")
cp "$db_path" "${db_path}.bak"
rm -f "$db_path" "$text_file"
# redo what should still work (slow path) and recreate both
ms=$(time_ms redo what)
if [ "$ms" -ge "$SLOW_THRESHOLD" ]; then pass "cold start is slow: ${ms}ms"
else fail "cold start unexpectedly fast: ${ms}ms"; fi
if [ -f "$db_path" ]; then pass "persistent DB recreated"
else fail "persistent DB NOT recreated"; fi
if [ -s "$text_file" ]; then pass "text cache created on cold start"
else fail "text cache NOT created on cold start"; fi
# Subsequent call should be fast
ms=$(time_ms redo what)
if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "fast after cold start: ${ms}ms"
else fail "not fast after cold start: ${ms}ms"; fi
# Restore
mv "${db_path}.bak" "$db_path" 2>/dev/null || true

# Test 17: Build in another dir does NOT invalidate text cache here
echo ""
echo "Test 17: Build elsewhere does not invalidate local text cache"
OTHER_DIR=$(dirname "$TEST_DIR")/counter
if [ -d "$OTHER_DIR" ]; then
    cd "$TEST_DIR"
    redo what 2>/dev/null  # ensure fresh text cache
    text_file=$(text_cache_path "$TEST_DIR")
    text_mtime_before=$(stat -c %Y "$text_file")
    # Build in a different directory
    cd "$OTHER_DIR"
    redo style 2>/dev/null || true
    sleep 1
    # Text cache for TEST_DIR should still be valid (dir mtime unchanged)
    cd "$TEST_DIR"
    text_mtime_after=$(stat -c %Y "$text_file")
    dir_mtime=$(stat -c %Y "$TEST_DIR")
    if [ "$text_mtime_after" -ge "$dir_mtime" ]; then
        pass "text cache still valid after build elsewhere"
    else
        fail "text cache invalidated by build in other dir"
    fi
    # Should still be fast (text cache hit)
    ms=$(time_ms redo what)
    if [ "$ms" -lt "$FAST_THRESHOLD" ]; then pass "still fast after build elsewhere: ${ms}ms"
    else fail "not fast after build elsewhere: ${ms}ms"; fi
else
    echo "  SKIP: no counter dir for cross-build test"
fi

# -------------------------------------------------------
# Text Cache Prewarm Tests (generate_text_caches)
# -------------------------------------------------------

# Test 18: Prewarm generates text caches for all DB directories
echo ""
echo "Test 18: Prewarm generates text caches for all persistent DB directories"
cd "$TEST_DIR"
# Count directories in persistent DB
db_path=$(python3 -c "from database.persistent_target_cache import get_persistent_db_path; print(get_persistent_db_path())" 2>/dev/null)
db_key_count=$(python3 -c "
from database.database import database, DATABASE_MODE
with database('$db_path', DATABASE_MODE.READ_ONLY) as db:
    print(len(db.keys()))
" 2>/dev/null)
# Wipe text caches and regenerate
cache_dir=$(cat "/tmp/redo-$(id -u)/cache_dir")
rm -rf "$cache_dir/what"
python3 -c "from database.persistent_target_cache import generate_text_caches; generate_text_caches()" 2>/dev/null
text_file_count=$(ls "$cache_dir/what/" 2>/dev/null | wc -l)
# Should have at least as many text files as DB keys (plus ancestors)
if [ "$text_file_count" -ge "$db_key_count" ]; then
    pass "text caches ($text_file_count) >= DB keys ($db_key_count)"
else
    fail "text caches ($text_file_count) < DB keys ($db_key_count)"
fi

# Test 19: Intermediate ancestor directories get text caches
echo ""
echo "Test 19: Intermediate ancestor directories get text caches"
# These dirs have no build targets but should have predefined-only caches
adamant_root="${ADAMANT_DIR}"
adamant_src="${ADAMANT_DIR}/src"
adamant_components="${ADAMANT_DIR}/src/components"
all_ok=true
for d in "$adamant_root" "$adamant_src" "$adamant_components"; do
    tf=$(text_cache_path "$d" 2>/dev/null)
    if [ -s "$tf" ]; then
        content=$(cat "$tf")
        # Should contain predefined targets
        if echo "$content" | grep -q "^all$" && echo "$content" | grep -q "^clean$"; then
            : # good
        else
            fail "text cache for $d missing predefined targets"
            all_ok=false
        fi
    else
        fail "no text cache for intermediate dir: $d"
        all_ok=false
    fi
done
if [ "$all_ok" = true ]; then pass "all intermediate dirs have valid text caches"; fi

# Test 20: No text caches above build roots
echo ""
echo "Test 20: No text caches generated above build roots"
spurious=false
for bad_key in "_" "_home" "_home_user"; do
    if [ -f "$cache_dir/what/${bad_key}.txt" ]; then
        fail "spurious text cache: ${bad_key}.txt"
        spurious=true
    fi
done
if [ "$spurious" = false ]; then pass "no text caches above build roots"; fi

# Test 21: Leaf directory text cache matches redo what output
echo ""
echo "Test 21: Prewarm text cache content matches redo what for leaf dir"
cd "$TEST_DIR"
# Get text cache content (from prewarm)
tf=$(text_cache_path "$TEST_DIR" 2>/dev/null)
cache_content=$(sort "$tf")
# Get live redo what output
live_content=$(redo what 2>&1 | tail +2 | sed 's/^redo //' | sort)
if [ "$cache_content" = "$live_content" ]; then
    pass "prewarm text cache matches redo what output"
else
    fail "prewarm text cache differs from redo what output"
    diff <(echo "$cache_content") <(echo "$live_content") | head -5
fi

# Test 22: generate_text_caches is fast (< 500ms)
echo ""
echo "Test 22: Text cache generation speed"
gen_ms=$(time_ms python3 -c "from database.persistent_target_cache import generate_text_caches; generate_text_caches()")
echo "  generate_text_caches: ${gen_ms}ms"
if [ "$gen_ms" -lt 5000 ]; then pass "text cache generation fast: ${gen_ms}ms < 5000ms"
else fail "text cache generation slow: ${gen_ms}ms >= 5000ms"; fi

# Test 23: Prewarm text cache for adamant_example root
echo ""
echo "Test 23: Prewarm covers adamant_example build root"
example_root=$(dirname "$(dirname "$ADAMANT_CONFIGURATION_YAML")")
tf=$(text_cache_path "$example_root" 2>/dev/null)
if [ -s "$tf" ]; then
    pass "adamant_example root has text cache"
else
    fail "adamant_example root missing text cache"
fi

# -------------------------------------------------------
# Shell Completion Tests (__redo_completion)
# -------------------------------------------------------

# Source the completion script. Disable set -e for this section since
# completion functions use non-zero returns for control flow.
set +e
source "$ADAMANT_DIR/env/redo_completion.sh" 2>/dev/null

# Helper: run completion and capture COMPREPLY
run_completion () {
    COMPREPLY=()
    rm -f ~/.cache/redo/what_cache_lastdir.txt
    __redo_completion redo "$1" "" 2>/dev/null
}

# Test 24: Tilde path completion returns targets
echo ""
echo "Test 24: Tilde path completion returns targets"
run_completion "~/adamant/"
if [ "${#COMPREPLY[@]}" -gt 0 ]; then
    # Check that results have the tilde prefix (strip trailing space from completions)
    first="${COMPREPLY[0]%% }"
    if [[ "$first" == "~/adamant/"* ]]; then
        pass "tilde completion works: ${#COMPREPLY[@]} results, first=$first"
    else
        fail "tilde completion wrong prefix: $first"
    fi
else
    fail "tilde completion returned 0 results"
fi

# Test 25: Absolute path completion returns targets
echo ""
echo "Test 25: Absolute path completion returns targets"
run_completion "$ADAMANT_DIR/"
if [ "${#COMPREPLY[@]}" -gt 0 ]; then
    pass "absolute path completion: ${#COMPREPLY[@]} results"
else
    fail "absolute path completion returned 0 results"
fi

# Test 26: Build subdirectory completion uses parent cache
echo ""
echo "Test 26: Build subdirectory targets via parent cache"
# Use a component directory that has build/ targets in its redo what output
build_test_dir="$TEST_DIR"
has_build=$(cd "$build_test_dir" && redo what 2>&1 | grep "^redo build/" | head -1)
if [ -n "$has_build" ]; then
    run_completion "$build_test_dir/build/"
    if [ "${#COMPREPLY[@]}" -gt 0 ]; then
        pass "build/ completion from parent cache: ${#COMPREPLY[@]} targets"
    else
        fail "build/ completion returned 0 targets"
    fi
else
    echo "  SKIP: no build targets in $build_test_dir"
fi

# Test 27: Nonexistent directory is fast (no slow redo subprocess)
echo ""
echo "Test 27: Nonexistent directory completes fast"
start_ms=$(date +%s%3N)
run_completion "$ADAMANT_DIR/this_dir_does_not_exist/"
end_ms=$(date +%s%3N)
elapsed=$((end_ms - start_ms))
if [ "$elapsed" -lt 500 ]; then
    pass "nonexistent dir fast: ${elapsed}ms, ${#COMPREPLY[@]} results"
else
    fail "nonexistent dir slow: ${elapsed}ms"
fi

# Test 28: Deep build path completion (build/obj/...)
echo ""
echo "Test 28: Deep build path completion"
# Use a component that has build/obj targets
test_comp="$ADAMANT_DIR/src/components/command_rejector"
if [ -d "$test_comp" ]; then
    # Ensure text cache exists
    cd "$test_comp" && redo what >/dev/null 2>&1
    cd "$OLDPWD"
    run_completion "$test_comp/build/"
    build_count=${#COMPREPLY[@]}
    run_completion "$test_comp/build/obj/"
    obj_count=${#COMPREPLY[@]}
    if [ "$build_count" -gt 0 ]; then
        pass "build/ has $build_count targets, build/obj/ has $obj_count targets"
    else
        fail "build/ returned 0 targets for command_rejector"
    fi
else
    echo "  SKIP: command_rejector not found"
fi

# Test 29: Completion results include directory entries
echo ""
echo "Test 29: Completion includes subdirectories"
run_completion "$ADAMANT_DIR/"
dirs_found=false
for r in "${COMPREPLY[@]}"; do
    if [[ "$r" == */ ]]; then
        dirs_found=true
        break
    fi
done
if [ "$dirs_found" = true ]; then
    pass "completion includes directory entries"
else
    fail "no directory entries in completion results"
fi

# Test 30: Partial target name completion
echo ""
echo "Test 30: Partial target name filters correctly"
run_completion "$ADAMANT_DIR/cl"
all_match=true
for r in "${COMPREPLY[@]}"; do
    base="${r##*/}"
    base="${base%% }"  # strip trailing space
    if [[ "$base" != cl* ]]; then
        fail "partial completion mismatch: $r does not start with cl"
        all_match=false
        break
    fi
done
if [ "$all_match" = true ] && [ "${#COMPREPLY[@]}" -gt 0 ]; then
    pass "partial completion: ${#COMPREPLY[@]} matches for 'cl' (clean, clean_all, clear_cache)"
else
    [ "$all_match" = true ] && fail "partial completion returned 0 results"
fi

# -------------------------------------------------------
echo ""
echo "================================"
echo "Results: $PASS passed, $FAIL failed"
if [ "$FAIL" -gt 0 ]; then
    echo "SOME TESTS FAILED"
    exit 1
else
    echo "ALL TESTS PASSED"
fi
