name=`basename $2`
redo-ifchange build/template/$name

echo '
with Ada.Text_IO; use Ada.Text_IO;
with Aa.Representation;
with Bb.Representation;
'
cat build/template/$name | awk '

BEGIN {
  declaration_count=0;
  statement_count=0;
}

/TODO declarations/ {
  if( declaration_count == 0 ) {
    declaration_count++;
  }
  else if( declaration_count == 1 ) {
    declaration_count++;
  }
  else {
    print "      Ignore : Natural;"
  }
  next
}

/TODO statements/ {
  if( statement_count == 0 ) {
    print "      Put_Line (\"Got: \" & Aa.Representation.Image (Arg));"
    print "      Self.Aa_T_Send (Arg);"
    statement_count++;
  }
  else if( statement_count == 1 ) {
    print "      Put_Line (\"Got: \" & Bb.Representation.Image (Arg));"
    print "      Self.Bb_T_Send (Arg);"
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
