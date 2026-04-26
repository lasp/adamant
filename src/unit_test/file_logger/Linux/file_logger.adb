with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
with String_Util;
with Safe_Deallocator;

package body File_Logger is

   -- Each Instance gets its own File_Type so multiple loggers can
   -- coexist (e.g. nested test runners or future per-component logs).
   -- The incomplete type in the spec is completed here so the host's
   -- Ada.Text_IO.File_Type does not have to leak into the shared spec.
   type File_Holder is record
      File : Ada.Text_IO.File_Type;
   end record;

   -- The host File_Logger resolves a scenario name (passed by the test
   -- scaffold) into an absolute path under <test_dir>/log/<name>.log,
   -- back-computed from the running binary's location. This keeps the
   -- template target-agnostic -- only this body knows how to find the
   -- filesystem.
   function Resolve_Log_Path (Name : String) return String is
      Bin_Path  : constant String := Ada.Command_Line.Command_Name;
      Bin_Dir   : constant String := Containing_Directory (Bin_Path);            -- build/bin/<TARGET>
      Build_Dir : constant String := Containing_Directory (Bin_Dir);             -- build/bin
      Test_Root : constant String := Containing_Directory (Build_Dir);           -- build
   begin
      return Test_Root & "/log/" & Name & ".log";
   end Resolve_Log_Path;

   procedure Open (Self : in out Instance; File_Directory : in String) is
      Path : constant String := Resolve_Log_Path (File_Directory);
   begin
      -- Check that the directory exists first. If not, create it before creating the file
      if not Exists (Containing_Directory (Path)) then
         Create_Directory (Containing_Directory (Path));
      end if;

      -- Allocate per-Instance storage on first Open. Re-Open on the
      -- same Instance reuses the holder (after the prior Close
      -- released the File_Type).
      if Self.Holder = null then
         Self.Holder := new File_Holder;
      end if;

      -- Assert that the file is not yet open before attempting to open it.
      pragma Assert (not Is_Open (Self.Holder.File));
      -- Open the file we are going to write
      Create (Self.Holder.File, Out_File, Path);
      Self.Active := True;
   end Open;

   procedure Log (Self : in Instance; String_To_Write : in String) is
      Utc_Epoch : constant Ada.Calendar.Time :=
         Time_Of (Year => 1_970, Month => 1, Day => 1, Hour => 0,
                  Minute => 0, Second => 0, Sub_Second => 0.0,
                  Leap_Second => False);
      Time_Difference : constant Duration := Clock - Utc_Epoch;
   begin
      -- Make sure the log is open before attempting to write to it
      if Self.Active and then Self.Holder /= null and then Is_Open (Self.Holder.File) then
         -- Then write what was passed in plus the UTC time
         Put_Line (Self.Holder.File,
                   String_Util.Trim_Both (Duration'Image (Time_Difference))
                   & " " & String_To_Write);
      end if;
   end Log;

   procedure Close (Self : in out Instance) is
      -- Use Safe_Deallocator instead of Ada.Unchecked_Deallocation directly:
      -- this Linux/ body compiles for both Linux_Test (no restriction, real
      -- free) and Linux_Debug (ravenscar.adc enforces No_Unchecked_Deallocation,
      -- where Free_If_Testing is a no-op).
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing
        (Object => File_Holder, Name => File_Holder_Access);
   begin
      if Self.Holder /= null then
         if Is_Open (Self.Holder.File) then
            -- Close the file after logging is complete.
            Close (Self.Holder.File);
         end if;
         -- Release the per-Instance file state. Re-Open will allocate fresh.
         Free_If_Testing (Self.Holder);
      end if;
      Self.Active := False;
   end Close;

end File_Logger;
