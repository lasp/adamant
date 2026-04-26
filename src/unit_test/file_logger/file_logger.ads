-- Test logger with target-specific bodies (Linux/, bb/). The Linux variant
-- writes to a file on disk and stamps every line with seconds-since-epoch.
-- The bareboard variant (no filesystem available) routes Log to
-- Ada.Text_IO, which the runtime maps to UART. Public Instance state is
-- kept target-agnostic so the spec is shared.
--
-- Per-Instance file state lives behind an incomplete-type access
-- (File_Holder) whose full type is defined only in the target body.
-- This lets multiple loggers coexist on Linux (each Instance owns its
-- own File_Type) without leaking Ada.Text_IO.File_Type into the spec
-- which would not compile on bareboard, where the stripped Text_IO
-- has neither File_Type nor Standard_Error.
package File_Logger is

   type Instance is tagged limited private;
   type Instance_Access is access all Instance;
   procedure Open (Self : in out Instance; File_Directory : in String);
   procedure Log (Self : in Instance; String_To_Write : in String);
   procedure Close (Self : in out Instance);

private

   type File_Holder;
   type File_Holder_Access is access File_Holder;

   type Instance is tagged limited record
      -- True between Open and Close; gates Log writes.
      Active : Boolean := False;
      -- Per-Instance file state. Concrete type defined in the target
      -- body: Linux holds an Ada.Text_IO.File_Type; bareboard never
      -- allocates this (Holder stays null) and routes Log straight
      -- to Put_Line / UART using just Active.
      Holder : File_Holder_Access := null;
   end record;

end File_Logger;
