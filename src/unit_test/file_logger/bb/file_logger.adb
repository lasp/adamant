with Ada.Text_IO;

package body File_Logger is

   -- No filesystem on bareboard. Log routes to single-arg Put_Line,
   -- which an embedded runtime usually maps to a UART
   -- character device. The Holder access is never allocated on
   -- bareboard. Self.Active alone gates Log writes. Defining
   -- File_Holder here as a null record completes the spec's
   -- incomplete type so the package compiles.
   type File_Holder is null record;

   procedure Open (Self : in out Instance; File_Directory : in String) is
      pragma Unreferenced (File_Directory);
   begin
      -- File_Directory is a scenario name, not a filesystem path -- the
      -- bareboard logger has no concept of either. We just enable
      -- logging so subsequent Log calls reach UART.
      Self.Active := True;
   end Open;

   procedure Log (Self : in Instance; String_To_Write : in String) is
   begin
      if Self.Active then
         -- Routes to UART via the runtime's character output.
         Ada.Text_IO.Put_Line (String_To_Write);
      end if;
   end Log;

   procedure Close (Self : in out Instance) is
   begin
      Self.Active := False;
   end Close;

end File_Logger;
