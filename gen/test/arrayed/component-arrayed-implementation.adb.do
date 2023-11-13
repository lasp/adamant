name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
/TODO declarations/ {
  print "      Arg_Copy : Aa.T := Arg;"
  print "      Arg_Ret : Bb.T;"
  print "      Ignore_Arg_Ret : Bb.T;"
}
/TODO statements/ {
  print "      for I in Component.Arrayed.Aa_T_Request_Index loop"
  print "         Self.Aa_T_Send (I, Arg);"
  print "         Ignore_Arg_Ret := Self.Aa_T_Request (I, Arg);"
  print "         Arg_Ret := Self.Bb_T_Get (I);"
  print "         Self.Aa_T_Provide (I, Arg_Copy);"
  print "      end loop;"

  print ""
  print "      Put (\" Sending on n-arrayed connectors: \" & Integer@Image (Self.Connector_Aa_T_Request_2@Length));"
  print "      Put_Line (\" times.\");"
  print "      for I in Self.Connector_Aa_T_Request_2@Range loop"
  print "         Self.Aa_T_Send_2 (I, Arg);"
  print "         Ignore_Arg_Ret := Self.Aa_T_Request_2 (I, Arg);"
  print "         Arg_Ret := Self.Bb_T_Get_2 (I);"
  print "         Self.Aa_T_Provide_2 (I, Arg_Copy);"
  print "      end loop;"
  print "      pragma Unreferenced (Arg_Ret);"
  next
}

{
  print $0
}
' | tr "@" "'"
