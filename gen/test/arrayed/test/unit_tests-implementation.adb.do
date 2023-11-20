name=`basename $2`
redo-ifchange build/template/$name

cat build/template/$name | awk '
/with AUnit.Assertions/ {
  print "with Ada.Text_IO; use Ada.Text_IO;"
  print "with Aa.Representation;"
  print "with Smart_Assert;"
  next
}
/TODO declarations/ {
  print "      Data_A : constant Aa.T := (One => 17, Two => 23, Three => 5);"
  print "      package Positive_Assert is new Smart_Assert.Discrete (Positive, Positive@Image);"
  print "      package A_Assert is new Smart_Assert.Basic (Aa.T, Aa.Representation.Image);"
}
/TODO replace/ {
  print "      Put_Line (\"Sending data on connectors... \");"
  print "      Self.Tester.Aa_T_Send (Data_A);"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Recv_Sync_History.Get_Count, 4);"
  print "      for Index in 1 .. 4 loop"
  print "         A_Assert.Eq (Data_A, Self.Tester.Aa_T_Recv_Sync_History.Get (Index));"
  print "      end loop;"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Service_History.Get_Count, 4);"
  print "      Positive_Assert.Eq (Self.Tester.Bb_T_Return_History.Get_Count, 4);"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Modify_History.Get_Count, 4);"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Recv_Sync_2_History.Get_Count, 3);"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Service_2_History.Get_Count, 3);"
  print "      Positive_Assert.Eq (Self.Tester.Bb_T_Return_2_History.Get_Count, 3);"
  print "      Positive_Assert.Eq (Self.Tester.Aa_T_Modify_2_History.Get_Count, 3);"
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
' | tr "@" "'" | sed 's/-- Self.Tester.init_Base/Self.Tester.init_Base/g' | sed 's/=> TBD/=> 3/g'
