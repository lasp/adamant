#!/bin/bash

# The function that provides bash completions for the redo command.
# To see debug messages, set DBG=true.
__redo_completion () {
    local underscore=$_ # must be very first command
    local special_arg=____redo_completion_special_arg
    local first_run=false
    [ "$underscore" != "$special_arg" ] && first_run=true


    # $_ is set to the argument of the last executed command
    # so on the first run, it will depend on what the user just ran
    # the second run onwards, it will be set to $special_arg
    # this function runs once after every tab completion,
    # so we can use this to detect whether this is the user's first tab press


    dbg () {
        local prevstatus=$?
        [ "$DBG" = true ] &&  echo - "$@"
        return $prevstatus
    }

    dbg
    dbg "> start: first_run=$first_run this_word=$this_word"


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

    # main cache: stores output of previous runs of `redo what`
    words_cache_ok () {
        local path=$1
        [ -f "$path"/build/redo/what_cache.txt ] || return 1
        [ -n "$(cat "$path"/build/redo/what_cache.txt)" ] || return 1
        # todo: check for file integrity, timestamp, etc?
        return 0
    }

    words_cache_save () {
        local path=$1
        local res status
        res="$(what_parsed "$path")"
        status=$?
        echo "$res"
        [ "$status" = 0 ] || return 2
        mkdir -p "$path"/build/redo && echo "$res" > "$path"/build/redo/what_cache.txt
        [ "$?" = 0 ] || return 1
        return 0
    }

    # short lived cache of a directories in which we've run `redo what`
    # i.e. can we assume that the cache in this directory is completely up to date?
    dir_first_visit () {
        local path=$1 filename=~/.cache/redo/what_cache_dirs.txt
        [ -f "$filename" ] || return 0

        cat "$filename" | grep "^$path\$" >/dev/null && return 1
        return 0
    }

    dir_cache_save () {
        local path=$1
        mkdir -p ~/.cache/redo/ 2>/dev/null && echo "$path" >> ~/.cache/redo/what_cache_dirs.txt
    }

    # doesn't take a path, since dir cache is always relative to ./
    dir_cache_clean () {
        local filename=~/.cache/redo/what_cache_dirs.txt
        [ -f "$filename" ] && rm "$filename"
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
            if [ "$LEAD_DIR" != build ] && [ -d "$basepath/$LEAD_DIR" ]; then
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
        if [ $path != . ] && [ -n "$RES" ]; then
            RES=$(echo "$RES" | prepend "$path/")
        fi
    }

    append_compgen_dirs () {
        # match against directories in $path
        local full_arg
        if [ "$path" = . ]; then
            full_arg=$arg
        # elif [ -z "$arg" ]; then
            # full_arg=$path
        else
            full_arg=$path/$arg
        fi

        dbg full arg: "$full_arg"

        # stupid way of appending lines to variable
        RES=$(echo "$RES
$(compgen -d "$full_arg" | grep -v '\bbuild\b' | sed 's/$/\//')" | grep -v '^$')
    }

    synchronous_work () {
        compgen_arg=$(words_cache_save "$path")
        RES=$(compgen -W "$compgen_arg" "$arg")
        prepend_path
        append_compgen_dirs
    }

    async_work () {
        words_cache_save "$path" >/dev/null
    }

    should_recurse () {
        try_trim_leading_dir "$path" "$arg"
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
        local path=$1 arg=$2

        # return value -- must reset at start of function
        RES=

        # clean up $path / $arg -- replace instances of ./ with nothing
        path=$(echo "$path" | sed 's/\b\.\///')
        arg=$(echo "$arg" | sed 's/\b\.\///')
        [ "$arg" = . ] && arg=./ # just bc the user must have typed it in

        dbg "> enter helper: path=($path) arg=($arg)"

        if what_predef_parsed "$path" >/dev/null; then
            # path only gets edited when we recurse, so this is safe
            save_last_confirmed_dir "$path"
        else
            # TODO: make completion work inside of /build directory
            # for now just dip
            dbg '> redo what unavailable'
            if pwd | grep '/build$' >/dev/null; then
                dbg '> stuck in build/ so dip'
                return 0
            fi

            # `redo what` isn't even available
            # so just complete dirs
            append_compgen_dirs
            dbg new res just dropped "RES ($RES)"
            # return 1

            if should_recurse; then
                dbg '> yet recurse'
                do_recurse
                return $?
            fi
            return 0
        fi


        # locals
        local compgen_arg first_run=false
        if dir_first_visit "$path"; then
            first_run=true
            dir_cache_save "$path"
        fi

        # overview:
        # if typed `redo <tab>` or `redo path/<tab>`, run `redo what` synchronously
        # check word cache (fallback to `redo what_predefined`)
        # -> if match, use the result and update caches in background
        # if no matches yet, check for redo's path syntax
        # -> if match, recurse using subdirectory
        # if still no matches, match against directory names
        # if *still* no matches, and cache is outdated, synchronously run `redo what`
        # return regardless of matches


        # I assume that if someone types `redo <tab>`,
        # they probably want an up-to-date list of commands
        # but if someone types `redo b<tab>` for example,
        # they probably intended to autocomplete build/
        # which is likely still available, and useful to provide instantly

        # if typed `redo <tab>` and cache is outdated, bypass cache
        if [ "$first_run" = true ] && [ -z "$arg" ]; then
            synchronous_work
            append_compgen_dirs
            return 0
        fi


        if words_cache_ok "$path"; then
            compgen_arg=$(cat "$path"/build/redo/what_cache.txt)
            dbg '> cache ok'
        else
            # fallback to `redo what_predefined`
            compgen_arg=$(what_predef_parsed "$path")
            dbg '> cache fallback to what_predef'
        fi

        # generate completions
        RES=$(compgen -W "$compgen_arg" "$arg")

        if [ -n "$RES" ]; then

            dbg '> used cache'

            prepend_path
            append_compgen_dirs
            (async_work &)

            return 0
        fi

        dbg '> cache miss'

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
            # no need to prepend_path
            (async_work &)
            return 0
        fi

        # now there's really nothing to match
        # so do synchronous anyway, but only on first run
        # this is useful for i.e. autocompleting build/ in a directory for the first time after making a .component.yaml file
        if [ "$first_run" = true ]; then
            dbg '> synchronous time'
            synchronous_work
            return 0
        fi
        dbg '> no matches at all'
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

    # helps us figure out whether re-running `redo what` is a waste
    # if first_run=true then the user might have run commands
    # which might change the output of `redo what`
    if [ $first_run = true ]; then
        dir_cache_clean
    fi

    local cmdname=$1 this_word=$2 prev_word=$3
    local starting_dir=.

    # manually detect whether "~" is used
    # TODO: incompatible with ~otheruser/ syntax
    local tilde=false
    if echo "$this_word" | grep '^~' >/dev/null; then
        tilde=true
        this_word=$(echo "$this_word" | sed "s#~#$HOME#")
    fi

    # call helper function

    # helper function sets the RES variable
    if [ "$first_run" = false ] && try_cached_dir; then
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

    unset -f redo_cmd_parsed what_parsed what_predef_parsed words_cache_ok words_cache_save dir_first_visit dir_cache_save dir_cache_clean save_last_confirmed_dir get_last_confirmed_dir prepend try_trim_leading_dir append_compgen_dirs synchronous_work async_work prepend_path redo_completion_helper try_cached_dir

    # must be very last command, see top
    echo $special_arg > /dev/null
}

# the reason we can't use `complete -o dirnames` is that
# we need to exclude ./build (and want to exclude hidden dirs)
complete -o nospace -o nosort -F __redo_completion redo
