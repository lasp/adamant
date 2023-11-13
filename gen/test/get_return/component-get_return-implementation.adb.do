name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
/TODO statements/ {
  print "      To_Return.One := 1;"
  print "      To_Return.Two := 20;"
  print "      To_Return.Three := 7;"
  print "      Put_Line (\"Component returning A to get: (\" & Integer@Image (To_Return.One) & \", \" & Integer@Image (To_Return.Two) & \", \" & Integer@Image (To_Return.Three) & \")\");"
  next
}

{
  print $0
}
' | tr "@" "'"
