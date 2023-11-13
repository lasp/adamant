name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '
/null; -- TODO statements/ {
  print "      Put_Line (\"Receiving at index: \" & Connector_Index_Type@Image (Index));"
  print "      Self.Packed_Connector_Index_T_Send ((Index => Index));"
  print "      Self.Aa_T_Send (Arg);"
  next
}

/return To_Return;/ {
  print "      Put_Line (\"Receiving at index: \" & Connector_Index_Type@Image (Index));"
  print "      Self.Packed_Connector_Index_T_Send ((Index => Index));"
  print "      return To_Return;"
  next
}


{
  print $0
}
' | tr "@" "'"
