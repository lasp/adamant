with Ada.Text_IO;

package body Smart_Assert_Error_Io is
   procedure Put_Line (S : in String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, S);
   end Put_Line;
end Smart_Assert_Error_Io;
