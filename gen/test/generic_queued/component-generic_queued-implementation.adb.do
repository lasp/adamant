name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
BEGIN {
  count = 0;
}

/TODO declarations/ {
  if (count == 0) {
    print "      A_Out : Generic_Type_1;"
  }
}
/TODO statements/ {
  if (count == 0) {
    print "      Put_Line (\"Component: Received generic type on Generic_Type_2_Sync_In\");"
    print "      Self.Generic_Type_1_Send (A_Out);"
    count = count + 1;
    next
  }
  else {
    print "      pragma Assert (Self.Dispatch_All = 1, \"only one message\");"
  }
}

{
  print $0
}
'
