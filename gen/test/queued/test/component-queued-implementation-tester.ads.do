name=`basename "$0" | sed 's/.do//g'`
redo-ifchange build/template/$name
cp build/template/$name $3
