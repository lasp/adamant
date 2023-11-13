name=`basename $2`
redo-ifchange build/template/$name
cp build/template/$name $3
