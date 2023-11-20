name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
'
cat build/template/$name | awk '
/To_Return : Bb.T;/ {
  print "      A_Out : constant Aa.T := (One => Arg.Element, Two => Arg.Element2, Three => Arg.Element2);"
  print "      A_Ret : Aa.T := (One => Arg.Element, Two => Arg.Element2, Three => Arg.Element2);"
}
/TODO statements/ {
  print "      Put_Line (\"Component receiving something on B In\");"
  print "      Put_Line (\"Element: \");"
  print "      Put (Arg.Element);"
  print "      New_Line;"
  print "      Put_Line (\"Element 2: \");"
  print "      Put (Arg.Element2);"
  print "      New_Line;"
  print "      To_Return.Element := 1;"
  print "      To_Return.Element2 := 20;"
  print "      Put_Line (\"Component returning A to requester\");"
  print "      Put_Line (\"Element: \");"
  print "      Put (To_Return.Element);"
  print "      New_Line;"
  print "      Put_Line (\"Element 2: \");"
  print "      Put (To_Return.Element2);"
  print "      New_Line;"
  print "      A_Ret := Self.Aa_T_Request (A_Out);"
  print "      pragma Unreferenced (A_Ret);"
  next
}

{
  print $0
}
'
