redo what 2>&1 | grep "redo build" | grep -v build/obj | grep -v build/template | grep -v build/src | grep -v assembly_events_to_text | sed 's/redo //g' | xargs redo-ifchange
