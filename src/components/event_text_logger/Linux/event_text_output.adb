with Ada.Text_IO;

package body Event_Text_Output is

   procedure Put_Event_Line (Text : in String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Text);
   end Put_Event_Line;

end Event_Text_Output;
