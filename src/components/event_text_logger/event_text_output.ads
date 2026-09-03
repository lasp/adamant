-- Output sink for the event text logger with target-specific bodies.
-- Linux writes to Standard_Error so event text stays off the standard
-- output data path; the bareboard Text_IO subset has no Standard_Error,
-- so its body writes to the runtime console.
package Event_Text_Output is

   procedure Put_Event_Line (Text : in String);

end Event_Text_Output;
