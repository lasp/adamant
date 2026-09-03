--------------------------------------------------------------------------------
-- Event_Text_Logger Component Implementation Body
--------------------------------------------------------------------------------

with Event_Text_Output;
with Event_Types;

package body Component.Event_Text_Logger.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Events are received asynchronously on this connector:
   overriding procedure Event_T_Recv_Async (Self : in out Instance; Arg : in Event.T) is
      Ignore : Instance renames Self;
   begin
      Event_Text_Output.Put_Event_Line (Self.Event_To_Text.all (Arg));
   end Event_T_Recv_Async;

   overriding procedure Event_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Event.T) is
      Ignore : Instance renames Self;
   begin
      Event_Text_Output.Put_Event_Line ("Event with ID: " & Event_Types.Event_Id'Image (Arg.Header.Id) & " was dropped due to full queue!");
   end Event_T_Recv_Async_Dropped;

end Component.Event_Text_Logger.Implementation;
