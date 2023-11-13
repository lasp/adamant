With Ada.text_IO; use Ada.Text_io;

procedure Main is
 PROCEDURE print_hello(str : in String )    is
   begin
               put_Line ( "Hello, " & str & "!" );
    end;
BeGiN
print_hello("world");
end Main;
