#!/bin/bash

# The function that provides bash completions for the redo command.
# To see debug messages, set DBG=true.
__redo_completion () {
    dbg () {
        local prevstatus=$?
        [ "$DBG" = true ] &&  echo - "$@"
        return $prevstatus
    }

    dbg
    dbg "> start: this_word=$this_word"


    ##########################################
    # functions used in redo_completion_helper
    ##########################################

    # takes in an argument, likely `what` or `what_predefined`
    redo_cmd_parsed () {
        redo "$1" 2>&1 | tail +2 | sed 's/^redo //' | sed 's/\n/ /' | echo "$(cat -)
what" | grep -v "^$"
        return ${PIPESTATUS[0]} # i.e. status of redo what
    }

    # for all these functions:
    # path arg should/will not contain a trailing slash

    what_parsed () {
        local path=$1
        redo_cmd_parsed "$path/what"
    }

    what_predef_parsed () {
        local path=$1
        redo_cmd_parsed "$path/what_predefined"
    }

    # Fast text cache read for tab completion
    # Reads the breadcrumb written by Python's get_cache_dir() to find
    # the cache directory, then reads the per-directory text cache file.
    # Returns the cached targets on stdout, or fails (return 1) on miss.
    text_cache_read () {
        local path=$1
        local abs_dir
        # Resolve to absolute path without subshells when possible
        if [ "$path" = "." ]; then
            abs_dir=$PWD
        elif [[ "$path" == /* ]]; then
            abs_dir="$path"
        else
            abs_dir=$(cd "$path" 2>/dev/null && pwd) || return 1
        fi

        local breadcrumb="/tmp/redo-$(id -u)/cache_dir"
        local cache_dir
        read -r cache_dir < "$breadcrumb" 2>/dev/null || return 1

        # Encode directory path as filename: /foo/bar → _foo_bar
        # Must match Python's save_text_cache() encoding.
        local dir_key="${abs_dir//\//_}"
        local text_file="${cache_dir}/what/${dir_key}.txt"

        # Text cache must exist and be non-empty
        [ -s "$text_file" ] || return 1

        # Invalidate if directory has changed (files added/removed)
        local text_mtime dir_mtime
        text_mtime=$(stat -c %Y "$text_file" 2>/dev/null) || return 1
        dir_mtime=$(stat -c %Y "$abs_dir" 2>/dev/null) || return 1
        [ "$text_mtime" -ge "$dir_mtime" ] || return 1

        cat "$text_file"
        return 0
    }

    # shorter-lived cache to avoid having to recurse every new dir completion
    # see usages of `save_last_confirmed_dir` and `try_cached_dir`
    save_last_confirmed_dir () {
        local dir=$1
        dbg "saved $dir"
        mkdir -p ~/.cache/redo/ && echo "$dir" > ~/.cache/redo/what_cache_lastdir.txt
    }

    get_last_confirmed_dir () {
        local filename=~/.cache/redo/what_cache_lastdir.txt
        [ -f "$filename" ] || return 1

        local dir
        dir=$(cat "$filename")
        [ $? = 0 ] || return 1

        LAST_DIR="$dir"
        return 0
    }

    prepend () {
        while read -r line; do
            echo "$1$line"
        done
    }

    # if $1 is formatted dir///other//stuff,
    # split into LEAD_DIR=dir RES=other//stuff
    # also checks if $1/LEAD_DIR is a directory, not just for syntax
    try_trim_leading_dir () {
        local basepath=$1 string=$2
        local all_ifs=false
        LEAD_DIR=
        REST=
        if echo "$string" | grep '/' >/dev/null; then
            # lmao, the sed command is replace 1 or more slashes with just 1 slash
            LEAD_DIR=$(echo "$string" | sed 's/\/\/*/\//' | cut -d '/' -f 1)
            if [ -d "$basepath/$LEAD_DIR" ]; then
                all_ifs=true
                REST=$(echo "$string" | sed 's/\/\/*/\//' | cut -d '/' -f 2-)
            fi
        fi

        [ "$all_ifs" = true ] && return 0
        return 1
    }



    # the following functions are more like subroutines for redo_completion_helper
    # these assume lots of global state

    prepend_path () {
        if [ "$path" != . ] && [ -n "$RES" ]; then
            RES=$(echo "$RES" | prepend "$path/")
        fi
    }

    append_compgen_dirs () {
        # match against directories in $path
        local full_arg
        if [ "$path" = . ]; then
            full_arg=$arg
        else
            full_arg=$path/$arg
        fi

        dbg full arg: "$full_arg"

        # stupid way of appending lines to variable
        RES=$(echo "$RES
$(compgen -d "$full_arg" | sed 's/$/\//')" | grep -v '^$')
    }

    synchronous_work () {
        compgen_arg=$(what_parsed "$path")
        RES=$(compgen -W "$compgen_arg" "$arg")
        prepend_path
        append_compgen_dirs
    }

    should_recurse () {
        try_trim_leading_dir "$path" "$arg"
    }

    # When we already have a cached target list for a real redo directory,
    # treat slash-separated target prefixes as a virtual namespace and keep
    # narrowing the cached targets instead of probing synthetic subpaths like
    # build/what_predefined (which can be very slow to fail).
    try_virtual_recurse () {
        local cached_targets=$1
        local string=$2
        local normalized prefix suffix filtered

        VIRTUAL_LEAD_DIR=
        VIRTUAL_REST=
        VIRTUAL_FILTERED=

        echo "$string" | grep '/' >/dev/null || return 1

        normalized=$(echo "$string" | sed 's/\/\/*/\//')
        prefix=$(echo "$normalized" | cut -d '/' -f 1)
        suffix=$(echo "$normalized" | cut -d '/' -f 2-)

        [ -n "$prefix" ] || return 1

        filtered=$(echo "$cached_targets" | awk -v p="$prefix/" 'index($0, p) == 1 { print substr($0, length(p) + 1) }')
        [ -n "$filtered" ] || return 1

        VIRTUAL_LEAD_DIR=$prefix
        VIRTUAL_REST=$suffix
        VIRTUAL_FILTERED=$filtered
        return 0
    }

    do_recurse () {

        # these are set by try_trim_leading_dir
        local dir_prefix=$LEAD_DIR trimmed_arg=$REST

        dbg "arg ($arg) vs trimmed halves ($dir_prefix) ($trimmed_arg)"

        # keep paths as clean as possible to prevent stacking
        # potential todo: detect if user deliberately typed "." and if so, keep
        if [ "$path" = . ]; then
            path=$dir_prefix
        else
            path=$path/$dir_prefix
        fi

        dbg "> recurse: path ($path) arg ($trimmed_arg)"


        redo_completion_helper "$path" "$trimmed_arg"
        local status=$?

        return $status

    }



    # the main helper function
    redo_completion_helper () {
        local path=$1 arg=$2 cached_targets=$3

        # return value -- must reset at start of function
        RES=

        # clean up $path / $arg, strip leading ./ prefix
        [[ "$path" == ./* ]] && path="${path#./}"
        [ -z "$path" ] && path="."
        [[ "$arg" == ./* ]] && arg="${arg#./}"
        [ "$arg" = . ] && arg=./ # just bc the user must have typed it in

        dbg "> enter helper: path=($path) arg=($arg)"

        # Fast path: try text cache first (fast) before spawning any
        # Python subprocess (slower). A valid text cache proves this is a
        # redo directory, so we can skip what_predef_parsed entirely.
        local compgen_arg
        if [ -n "$cached_targets" ] || compgen_arg=$(text_cache_read "$path" 2>/dev/null); then
            if [ -n "$cached_targets" ]; then
                compgen_arg=$cached_targets
                dbg '> virtual target cache hit'
            else
                dbg '> text cache hit (fast path)'
            fi

            RES=$(compgen -W "$compgen_arg" "$arg")

            if [ -n "$RES" ]; then
                dbg '> matched'
                prepend_path
                append_compgen_dirs
                return 0
            fi

            if [ -n "$cached_targets" ]; then
                dbg '> virtual target cache miss for arg'
            else
                dbg '> text cache hit but no match for arg'
            fi

            # If the user is traversing a slash-separated target prefix inside
            # the cached target list, recurse within that virtual namespace
            # instead of probing synthetic redo paths like build/what_predefined.
            if try_virtual_recurse "$compgen_arg" "$arg"; then
                dbg "> virtual recurse: dir ($VIRTUAL_LEAD_DIR) arg ($VIRTUAL_REST)"
                redo_completion_helper "$path/$VIRTUAL_LEAD_DIR" "$VIRTUAL_REST" "$VIRTUAL_FILTERED"
                return $?
            fi

            # no match, so check for a directory prefix
            if should_recurse; then
                do_recurse
                return $?
            fi

            # fall through to dir completion
            RES=
            append_compgen_dirs
            return 0
        fi

        dbg '> text cache miss, trying slow path'

        # Slow path: no text cache. Check if this is a redo directory
        # by calling what_predef_parsed (spawns redo subprocess, tens of ms).
        if what_predef_parsed "$path" >/dev/null; then
            # path only gets edited when we recurse, so this is safe
            save_last_confirmed_dir "$path"
        else
            dbg '> redo what unavailable'

            # redo what is not available here, so just complete dirs
            append_compgen_dirs
            dbg new res just dropped "RES ($RES)"

            if should_recurse; then
                dbg '> yet recurse'
                do_recurse
                return $?
            fi
            return 0
        fi

        # No text cache — call redo what (also populates text cache)
        compgen_arg=$(what_parsed "$path")
        dbg '> called redo what (slow path)'

        # generate completions
        RES=$(compgen -W "$compgen_arg" "$arg")

        if [ -n "$RES" ]; then
            dbg '> matched'
            prepend_path
            append_compgen_dirs
            return 0
        fi

        dbg '> cache miss'

        # Reuse the freshly computed target list as a virtual namespace before
        # probing synthetic redo subpaths like build/what_predefined.
        if try_virtual_recurse "$compgen_arg" "$arg"; then
            dbg "> virtual recurse: dir ($VIRTUAL_LEAD_DIR) arg ($VIRTUAL_REST)"
            redo_completion_helper "$path/$VIRTUAL_LEAD_DIR" "$VIRTUAL_REST" "$VIRTUAL_FILTERED"
            return $?
        fi

        # no match, so check for a directory prefix
        if should_recurse; then
            # found directory, so recurse "into" it
            do_recurse
            return $?
        fi

        dbg '> no recurse'
        dbg pre-full arg: "path ($path) arg ($arg)"

        RES=
        append_compgen_dirs

        if [ -n "$RES" ]; then
            return 0
        fi

        # now there's really nothing to match
        # so do synchronous completion as a final fallback
        dbg '> synchronous time'
        synchronous_work
        return 0
    }



    # subroutine used shortly
    local subdir trimmed_arg
    try_cached_dir () {
        subdir= trimmed_arg=
        local all_ifs=false
        if get_last_confirmed_dir; then
            local lastdir=$LAST_DIR

            # check if arg starts with dir
            if [ "$lastdir" != . ] && echo "$this_word" | grep "^$lastdir/" >/dev/null; then
                subdir=$lastdir

                # the #*$ bit trims a prefix off the left string
                trimmed_arg=$(echo "${this_word#*$subdir}" | sed 's/^\///')
                all_ifs=true

                dbg "last confirmed: subdir=$subdir trimmed_arg=$trimmed_arg"
            fi
        fi
        [ "$all_ifs" = true ] && return 0 || return 1
    }



    ######################################
    # end definitions, start running stuff
    ######################################

    local cmdname=$1 this_word=$2 prev_word=$3
    local starting_dir=.

    # manually detect whether "~" is used
    # TODO: incompatible with ~otheruser/ syntax
    local tilde=false
    if echo "$this_word" | grep '^~' >/dev/null; then
        tilde=true
        this_word=$(echo "$this_word" | sed "s#~#$HOME#")
    fi

    # Handle absolute paths: split into a starting directory that has
    # a text cache (redo project dir) and the remainder as this_word.
    # Walk up from the deepest directory until we find a text cache,
    # so that "~/proj/src/comp/build/" becomes starting_dir=".../comp"
    # this_word="build/" and the existing compgen matches build/* targets.
    if [[ "$this_word" == /* ]]; then
        local abs_path="$this_word"
        local found_cache=false

        # Strip trailing slash for splitting, remember if it was there
        local trailing_slash=false
        [[ "$abs_path" == */ ]] && trailing_slash=true
        abs_path="${abs_path%/}"

        # Walk up directories to find one with a text cache
        local try_dir="$abs_path"
        while [ -n "$try_dir" ] && [ "$try_dir" != "/" ]; do
            if text_cache_read "$try_dir" >/dev/null 2>&1; then
                starting_dir="$try_dir"
                # Remainder is everything after starting_dir
                local remainder="${abs_path#$try_dir}"
                remainder="${remainder#/}"
                if [ "$trailing_slash" = true ] && [ -n "$remainder" ]; then
                    remainder="$remainder/"
                elif [ "$trailing_slash" = true ]; then
                    remainder=""
                fi
                this_word="$remainder"
                found_cache=true
                break
            fi
            try_dir="${try_dir%/*}"
        done

        # No cache found, fall back to simple split at last /
        if [ "$found_cache" = false ]; then
            if [[ "${this_word}" == */ ]]; then
                starting_dir="${this_word%/}"
                this_word=""
            else
                starting_dir="${this_word%/*}"
                this_word="${this_word##*/}"
            fi
            [ -z "$starting_dir" ] && starting_dir="/"
        fi
    fi

    # call helper function

    # helper function sets the RES variable
    if try_cached_dir; then
        # this saves a few recursions
        # see recursion in helper function for when this gets cached
        dbg "> insta-recurse"
        redo_completion_helper "$subdir" "$trimmed_arg"
    else
        redo_completion_helper "$starting_dir" "$this_word"
    fi

    if [ $? = 0 ]; then
        if [ -n "$RES" ]; then
            # if it ends with a slash, it's a path
            # so if it doesn't end with a slash, we should add a space to it
            RES=$(echo "$RES" | sed 's#$# #' | sed 's#/ $#/#')

            # reapply the tilde
            if [ "$tilde" = true ]; then
                RES=$(echo "$RES" | sed "s#^$HOME#~#")
            fi
        fi

        # split into bash array based on newline, not any whitespace (precaution)
        oldifs=$IFS
        IFS="
"
        COMPREPLY=($RES)
        IFS=$oldifs
        unset oldifs
    fi

    unset -f redo_cmd_parsed what_parsed what_predef_parsed text_cache_read save_last_confirmed_dir get_last_confirmed_dir prepend try_trim_leading_dir try_virtual_recurse append_compgen_dirs synchronous_work prepend_path redo_completion_helper try_cached_dir

}

# the reason we can't use `complete -o dirnames` is that
# we want to exclude hidden dirs
complete -o nospace -o nosort -F __redo_completion redo
