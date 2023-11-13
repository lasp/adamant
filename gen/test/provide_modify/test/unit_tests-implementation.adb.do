name=`basename $2`
redo-ifchange build/template/$name

cat build/template/$name | awk '
/with AUnit.Assertions/ {
  print "with Ada.Text_IO; use Ada.Text_IO;"
  print "with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;"
  print "with Aa.Representation, Bb;"
  print "with Smart_Assert;"
  next
}
/TODO declarations/ {
  print "      Data_A : constant Aa.T := (One => 4, Two => 20, Three => 20);"
  print "      Data_B : Bb.T := (Element => 4, Element2 => 20);"
  print "      package Positive_Assert is new Smart_Assert.Discrete (Positive, Positive@Image);"
  print "      package A_Assert is new Smart_Assert.Basic (Aa.T, Aa.Representation.Image);"
}
/TODO replace/ {
  print "      Self.Tester.Bb_T_Provide (Data_B);"
  print "      Put_Line (\"Tester main got B back\");"
  print "      Put_Line (\"Element: \");"
  print "      Put (Data_B.Element);"
  print "      New_Line;"
  print "      Put_Line (\"Element 2: \");"
  print "      Put (Data_B.Element2);"
  print "      New_Line;"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Modify_History.Get_Count, 1);"
  print "      A_Assert.Eq (Data_A, Self.Tester.Aa_T_Modify_History.Get (1));"
  next
}
/Assert \(False/ {
  next
}

{
  print $0
}
' | tr "@" "'" | sed "s/-- Self.Tester.Init_Base;/Self.Tester.Init_Base;/g"
