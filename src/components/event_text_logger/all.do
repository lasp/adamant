# All will not work in this directory since it depends on
# an assembly yaml. So just build the source.
redo what 2>&1 | grep "^redo build\/src" | sed 's/^redo //g'  | xargs redo-ifchange
