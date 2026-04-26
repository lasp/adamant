with Ada.Text_IO;

package body Smart_Assert_Error_Io is
   -- The embedded Ada.Text_IO variant has no File_Type and no
   -- Standard_Error. Single-arg Put_Line routes to the runtime's
   -- character output (usually UART for embedded runtime).
   procedure Put_Line (S : in String) is
   begin
      Ada.Text_IO.Put_Line (S);
   end Put_Line;
end Smart_Assert_Error_Io;
