name=`basename "$0" | sed 's/.do//g'`
redo-ifchange build/template/$name

cat build/template/$name | awk '
/To_Return : Aa.T;/ {
  print "      To_Return : constant Aa.T := Arg;"
  next
}
{
  print $0
}
' | tr "@" "'"
