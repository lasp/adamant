name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
'
cat build/template/$name | awk '
/TODO declarations/ {
  print "      A_Out : Aa.T := (One => Arg.Element, Two => Arg.Element2, Three => Arg.Element2);"
}
/TODO statements/ {
  print "      Put_Line (\"Component receiving something on B In\");"
  print "      Put_Line (\"Element: \");"
  print "      Put (Arg.Element);"
  print "      New_Line;"
  print "      Put_Line (\"Element 2: \");"
  print "      Put (Arg.Element2);"
  print "      New_Line;"
  print "      Arg.Element := 1;"
  print "      Arg.Element2 := 20;"
  print "      Put_Line (\"Component returning A to requester\");"
  print "      Put_Line (\"Element: \");"
  print "      Put (Arg.Element);"
  print "      New_Line;"
  print "      Put_Line (\"Element 2: \");"
  print "      Put (Arg.Element2);"
  print "      New_Line;"
  print "      Self.Aa_T_Provide (A_Out);"
  print "      pragma Unreferenced (A_Out);"
  next
}

{
  print $0
}
'
