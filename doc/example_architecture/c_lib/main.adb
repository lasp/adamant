with c_lib_h;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Data : aliased c_lib_h.c_data := (count => 0, limit => 3);
   Result : Unsigned_32;
begin
   for Idx in 0 .. 10 loop
      Result := Unsigned_32 (c_lib_h.increment (Data'Access));
   end loop;
   Put_Line (Unsigned_32'Image (Result));
end Main;
