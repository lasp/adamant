name=`basename $2`
redo-ifchange build/template/$name

cat build/template/$name | awk '
/with AUnit.Assertions/ {
  print "with Ada.Text_IO; use Ada.Text_IO;"
  print "with Aa.Representation;"
  print "with Bb.Representation;"
  print "with Bb;"
  print "with Smart_Assert;"
  print "with Basic_Assertions; use Basic_Assertions;"
  print "with Packed_Connector_Index.Assertion; use Packed_Connector_Index.Assertion;"
  next
}
/TODO declarations/ {
  print "      Data_A : constant Aa.T := (One => 17, Two => 23, Three => 5);"
  print "      Data_B : constant Bb.T := (Element => 4, Element2 => 20);"
  print "      package A_Assert is new Smart_Assert.Basic (Aa.T, Aa.Representation.Image);"
  print "      package B_Assert is new Smart_Assert.Basic (Bb.T, Bb.Representation.Image);"
}
/TODO replace/ {
  print "      Put_Line (\"Sending data on connectors... \");"
  print "      for Idx in Self.Tester.Connector_Aa_T_Send@Range loop"
  print "         Put_Line (\"Sending Aa_T_Send index \" & Natural@Image (Natural (Idx)));"
  print "         Self.Tester.Aa_T_Send (Idx, Data_A);"
  print "         Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);"
  print "         Natural_Assert.Eq (Self.Tester.Aa_Packed_Connector_Index_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         Natural_Assert.Eq (Self.Tester.Aa_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         A_Assert.Eq (Self.Tester.Aa_T_Recv_Sync_History.Get (Positive (Idx)), Data_A);"
  print "         Packed_Connector_Index_Assert.Eq (Self.Tester.Aa_Packed_Connector_Index_T_Recv_Sync_History.Get (Positive (Idx)), (Index => Idx));"
  print "      end loop;"
  print "      Self.Tester.Aa_T_Recv_Sync_History.Clear;"
  print "      Self.Tester.Aa_Packed_Connector_Index_T_Recv_Sync_History.Clear;"
  print "      for Idx in Self.Tester.Connector_Bb_T_Send@Range loop"
  print "         Put_Line (\"Sending Bb_T_Send_2_Index index \" & Natural@Image (Natural (Idx)));"
  print "         Self.Tester.Bb_T_Send (Idx, Data_B);"
  print "         Natural_Assert.Eq (Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Get_Count, Natural (Idx) - 1);"
  print "         Natural_Assert.Eq (Self.Tester.Bb_T_Recv_Sync_History.Get_Count, Natural (Idx) - 1);"
  print "         Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);"
  print "         Natural_Assert.Eq (Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         Natural_Assert.Eq (Self.Tester.Bb_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         B_Assert.Eq (Self.Tester.Bb_T_Recv_Sync_History.Get (Positive (Idx)), Data_B);"
  print "         Packed_Connector_Index_Assert.Eq (Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Get (Positive (Idx)), (Index => Idx));"
  print "      end loop;"
  print "      Self.Tester.Bb_T_Recv_Sync_History.Clear;"
  print "      Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Clear;"
  print "      for Idx in Self.Tester.Connector_Aa_T_Send@Range loop"
  print "         Put_Line (\"Sending Aa_T_Send index \" & Natural@Image (Natural (Idx)));"
  print "         Self.Tester.Aa_T_Send (Idx, Data_A);"
  print "         Put_Line (\"Sending Bb_T_Send_2_Index index \" & Natural@Image (Natural (Idx)));"
  print "         Self.Tester.Bb_T_Send (Idx, Data_B);"
  print "         Natural_Assert.Eq (Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Get_Count, Natural (Idx) - 1);"
  print "         Natural_Assert.Eq (Self.Tester.Bb_T_Recv_Sync_History.Get_Count, Natural (Idx) - 1);"
  print "         Natural_Assert.Eq (Self.Tester.Dispatch_All, 2);"
  print "         Natural_Assert.Eq (Self.Tester.Aa_Packed_Connector_Index_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         Natural_Assert.Eq (Self.Tester.Aa_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         A_Assert.Eq (Self.Tester.Aa_T_Recv_Sync_History.Get (Positive (Idx)), Data_A);"
  print "         Packed_Connector_Index_Assert.Eq (Self.Tester.Aa_Packed_Connector_Index_T_Recv_Sync_History.Get (Positive (Idx)), (Index => Idx));"
  print "         Natural_Assert.Eq (Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         Natural_Assert.Eq (Self.Tester.Bb_T_Recv_Sync_History.Get_Count, Natural (Idx));"
  print "         B_Assert.Eq (Self.Tester.Bb_T_Recv_Sync_History.Get (Positive (Idx)), Data_B);"
  print "         Packed_Connector_Index_Assert.Eq (Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Get (Positive (Idx)), (Index => Idx));"
  print "      end loop;"
  print "      Self.Tester.Aa_T_Recv_Sync_History.Clear;"
  print "      Self.Tester.Bb_Packed_Connector_Index_T_Recv_Sync_History.Clear;"
  print "      Self.Tester.Aa_Packed_Connector_Index_T_Recv_Sync_History.Clear;"
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
' | tr "@" "'" | sed 's/-- Self.Tester.Init_Base/Self.Tester.Init_Base/g' | sed 's/=> TBD/=> 3/g'
