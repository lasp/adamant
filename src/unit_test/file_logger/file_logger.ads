-- This is a generic, unprotected statistics data structure.
-- The user can instantiate this class with any type that they choose.
with Ada.Text_IO; use Ada.Text_IO;

package File_Logger is

   type Instance is tagged limited private;
   type Instance_Access is access all Instance;
   procedure Open (Self : in out Instance; File_Directory : in String);
   procedure Log (Self : in Instance; String_To_Write : in String);
   procedure Close (Self : in out Instance);

private

   type Instance is tagged limited record
      -- Internal file to use for writing to
      File : Ada.Text_IO.File_Type;
   end record;

end File_Logger;
