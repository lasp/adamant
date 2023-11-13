name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
/TODO declarations/ {
}
/TODO statements/ {
  print "      Put_Line (\"Component receiving something on Cc In\");"
  print "      Put_Line (\"Element: \" & Integer@Image (Arg.B.Element));"
  print "      Put_Line (\"Element 2: \" & Integer@Image (Arg.B.Element2));"
  print "      Self.Cc_T_Send (Arg);"
  next
}

{
  print $0
}
' | tr "@" "'"
