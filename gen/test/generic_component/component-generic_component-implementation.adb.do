name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
/TODO declarations/ {
  print "      A_out : Generic_Type_1;"
}
/TODO statements/ {
  print "      Put_Line (\"Component: Received generic type on Generic_Type_2_Sync_In\");"
  print "      Self.Generic_Type_1_Send (A_out);"
  next
}

{
  print $0
}
'
