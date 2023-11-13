name=`basename $2`
redo-ifchange build/template/$name

cat build/template/$name | awk '
/with AUnit.Assertions/ {
  print "with Ada.Text_IO; use Ada.Text_IO;"
  print "with Aa, Bb, Cc.Representation;"
  print "with Smart_Assert;"
  next
}
/TODO declarations/ {
  print "      Data_A : constant Aa.T := (One => 4, Two => 20, Three => 20);"
  print "      -- Data_A : Aa.T := (One => 4, Two => 20, Three => 20);"
  print "      Data_B : constant Bb.T := (Element => 4, Element2 => 20);"
  print "      Data_C : Cc.T;"
  print "      package Positive_Assert is new Smart_Assert.Discrete (Positive, Positive@Image);"
  print "      package C_Assert is new Smart_Assert.Basic (Cc.T, Cc.Representation.Image);"
}
/TODO replace/ {
  print "      Put_Line (\"Sending data on connectors... \");"
  print "      Data_C.A := Data_A;"
  print "      Data_C.B := Data_B;"
  print "      Self.Tester.Cc_T_Send (Data_C);"
  print "      Positive_Assert.Eq (Self.Tester.Cc_T_Recv_Sync_History.Get_Count, 1);"
  print "      C_Assert.Eq (Data_C, Self.Tester.Cc_T_Recv_Sync_History.Get (1));"
  print "      Put_Line (\"passed.\");"
  print "      New_Line;"
  next
}
/Assert \(False/ {
  next
}

{
  print $0
}
' | tr "@" "'" | sed 's/-- self.tester.init_Base;/self.tester.init_Base;/g'
