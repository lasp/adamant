name=`basename $2`
redo-ifchange build/template/$name

cat build/template/$name | awk '
/with AUnit.Assertions/ {
  print "with Ada.Text_IO; use Ada.Text_IO;"
  print "with Aa;"
  next
}
/TODO declarations/ {
  print "      Data_A : Aa.T := (One => 17, Two => 23, Three => 5);"
}
/TODO replace/ {
  print "      Data_A := Self.Tester.Aa_T_Get;"
  print "      Put_Line (\"Tester main got A back: (\" & Integer@Image (Data_A.One) & \", \" & Integer@Image (Data_A.Two) & \", \" & Integer@Image (Data_A.Three) & \")\");"
  print "      Put_Line (\"passed.\");"
  print "      New_Line;"
}
/Assert \(False/ {
  next
}

{
  print $0
}
' | tr "@" "'"
