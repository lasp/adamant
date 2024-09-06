--------------------------------------------------------------------------------
-- Event_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Event_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Get the timestamp:
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Send the first-tick event once:
      if not Self.First_Event_Sent then
         Self.Event_T_Send (Self.Events.First_Tick_Received (Timestamp));
         Self.First_Event_Sent := True;
      else
         -- Send the every-ten-ticks event every ten ticks:
         if (Self.Count mod 10) = 0 then
            Self.Event_T_Send (Self.Events.Ten_More_Ticks_Received (Timestamp, (Value => Self.Count)));
         end if;
      end if;

      -- Send the received event every time:
      Self.Event_T_Send (Self.Events.Tick_Received (Timestamp, Arg));

      -- Increment the count:
      Self.Count := @ + 1;
   end Tick_T_Recv_Sync;

end Component.Event_Component.Implementation;
