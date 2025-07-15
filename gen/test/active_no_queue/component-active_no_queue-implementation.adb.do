name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
'
cat build/template/$name | awk '

BEGIN {
  declaration_count=0;
  statement_count=0;
}

/TODO declarations/ {
  if( declaration_count == 0 ) {
    print "      A_Out : constant Aa.T := (One => Arg.Element, Two => Arg.Element2, Three => Arg.Element2);"
    declaration_count++;
  }
  else {
    print "      Ignore : Natural;"
  }
  next
}

/Ignore.*:.*Instance.*renames.*Self;/ {
  print "      pragma Annotate (GNATSAS, Intentional, \"subp always fails\", \"This is template code intentionally designed to fail to remind developer to implement it.\");"
  print $0
  next
}

/TODO statements/ {
  if( statement_count == 0 ) {
    print "      Put_Line (\"Component receiving something on B In\");"
    print "      Put_Line (\"Element: \" & Integer@Image (Arg.Element));"
    print "      Put_Line (\"Element 2: \" & Integer@Image (Arg.Element2));"
    print "      Self.Aa_T_Send (A_Out);"
    statement_count++;
  }
  else {
    print "      Put_Line (\"Sched called, dispatching from queue...\");"
    print "      Ignore := Self.Dispatch_All;"
  }
  next
}

{
  print $0
}
' | tr "@" "'"
