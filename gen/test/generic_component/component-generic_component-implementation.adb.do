name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
/TODO declarations/ {
  print "      pragma Warnings (Off, \"variable * is read but never assigned\");"
  print "      A_out : Generic_Type_1;"
  print "      pragma Warnings (On, \"variable * is read but never assigned\");"
  print "      pragma Annotate (GNATSAS, Intentional, \"unassigned variable\", \"Ignore, this is OK for this test\");"
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
