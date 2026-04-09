redo build/obj/Linux/system_registers.o 2>&1 | sed 's,\x1B\[[0-9;]*[a-zA-Z],,g' > $3 | true
