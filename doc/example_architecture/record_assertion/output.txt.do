redo-ifchange build/bin/Linux_Test/main.elf
build/bin/Linux_Test/main.elf 2>&1 | sed 's,\x1B\[[0-9;]*[a-zA-Z],,g' > $3 | true
