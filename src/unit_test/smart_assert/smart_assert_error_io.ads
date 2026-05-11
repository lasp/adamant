-- Print a single line to the test target's "error output".
--
-- Linux: routes to Ada.Text_IO.Standard_Error so assertion failure
-- messages survive `redo` (which swallows stdout). Bareboard: routes
-- to single-arg Ada.Text_IO.Put_Line, since the embedded Text_IO
-- variant has neither File_Type nor Standard_Error.
package Smart_Assert_Error_Io is
   procedure Put_Line (S : in String);
end Smart_Assert_Error_Io;
