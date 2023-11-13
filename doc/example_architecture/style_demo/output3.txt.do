redo pretty 2>&1 | sed 's,\x1B\[[0-9;]*[a-zA-Z],,g' | grep -v "done!" > $3 | true
