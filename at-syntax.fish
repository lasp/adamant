# requires moreutils and ripgrep


for i in 1 2
    set -l regex '^(\s*(\S.*?)\h+:=.*?(\h|\())\2((\.|\h|\)).*?;)'
    for file in (rg -P --multiline $regex -l)
        rg -P --multiline $regex -r '$1@$4' "$file" -N --passthru | sponge "$file"
    end
end
