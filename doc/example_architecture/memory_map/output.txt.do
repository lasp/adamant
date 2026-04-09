redo build/obj/Linux/nonvolatile_store.o 2>&1 | sed 's,\x1B\[[0-9;]*[a-zA-Z],,g' > $3 | true
