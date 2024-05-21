#!/bin/bash

# takes in an argument, likely `what` or `what_predefined`
__redo_cmd_parsed () {
    redo "$1" 2>&1 | tail +2 | sed 's/^redo //' | sed 's/\n/ /' | echo "$(cat -)
what" | grep -v "^$"
    return ${PIPESTATUS[0]} # i.e. status of redo what
}

__what_parsed () {
    __redo_cmd_parsed what
}

__what_predef_parsed () {
    __redo_cmd_parsed what_predefined
}

__do_caching () {
    local res status
    res="$(__what_parsed)"
    status=$?
    echo $res
    [ "$status" = 0 ] || return 2
    mkdir -p ./build/redo && echo "$res" > ./build/redo/whatcache.txt
    [ "$?" = 0 ] || return 1
    return 0
}


__cache_ok () {
    [ -f ./build/redo/whatcache.txt ] || return 1
    [ -n "$(cat ./build/redo/whatcache.txt)" ] || return 1
    # check for file integrity, timestamp, etc?
    return 0
}

__redo_completion () {
    # $_ is set to the argument of the last executed command
    # on the first run, it will depend on what the user just ran
    # the second run onwards, it will be set to $special_arg
    # this function runs once after every tab completion,
    # so we can use this to detect whether this is the user's first tab press

    local underscore=$_ # must be very first command
    local special_arg=____redo_completion_special_arg

    local first_run=false
    [ "$underscore" != "$special_arg" ] && first_run=true

    local cmdname=$1
    local this_word=$2
    local prev_word=$3

    # echo
    # echo cmdname = $cmdname
    # echo this_word = $this_word
    # echo prev_word = $prev_word
    # echo first_run = $first_run

    local compgen_arg compgen_res
    local do_synch=false


    # I assume that
    # if someone types `redo <tab>`,
    # they probably want an up-to-date list of commands
    # but if someone types `redo b<tab>` for example,
    # they probably intended to autocomplete build/
    # which is likely still available, and useful to provide instantly

    # if cache is unavailable,
    # or if typed `redo <tab>`
    # bypass cache
    if [ "$first_run" = true ] &&
        [ -z "$this_word" ] &&        # blank first arg
        [ "$prev_word" = "$cmdname" ] # no previous args
    then
        # do synchronous run
        do_synch=true
    else
        if __cache_ok; then
            compgen_arg=$(cat ./build/redo/whatcache.txt)
        else
            # fallback to `redo what_predefined`
            compgen_arg=$(__what_predef_parsed)
        fi

        # use cache for responsiveness
        # but make sure a background task is fetching up-to-date results


        compgen_res=$(compgen -W "$compgen_arg" "$this_word")

        # if nothing in cache matches, do synchronous anyway (but only on first run to make it responsive)
        if [ "$first_run" = true ] && [ -z "$compgen_res" ]; then
            do_synch=true
        elif ! ps p "$__redo_what_bg_pid" >/dev/null 2>&1; then
            # pid is not alive, so start bg process
            (__do_caching >/dev/null &)
            __redo_what_bg_pid=$!
        fi

    fi




    if [ $do_synch = true ]; then
        compgen_arg=$(__do_caching)
        compgen_res=$(compgen -W "$compgen_arg" "$this_word")
    fi


    # this variable is basically the return value for `complete`
    COMPREPLY=($compgen_res)

    # sets $_ -- see top of function
    echo $special_arg > /dev/null
}

complete -F __redo_completion redo

