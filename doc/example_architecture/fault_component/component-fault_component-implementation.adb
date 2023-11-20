--------------------------------------------------------------------------------
-- Fault_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Fault_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Get the system time:
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      -- Extract the seconds from the tick:
      Seconds : Interfaces.Unsigned_32 renames Arg.Time.Seconds;
   begin
      -- Check the timestamp in the tick. If it has zero seconds, this is unexpected and
      -- we should send a fault:
      if Seconds = 0 then
         Self.Fault_T_Send (Self.Faults.Zero_Time_Fault (Self.Sys_Time_T_Get));
      end if;

      -- If the last seconds value we received was not zero, then we
      -- should check for a time discontinuity. We expect the received
      -- time to be 1-2 seconds ahead of the last tick we received.
      if Self.Last_Received_Seconds /= Interfaces.Unsigned_32'First and then
          (Seconds > Self.Last_Received_Seconds + 2 or else
            Seconds < Self.Last_Received_Seconds + 1)
      then
         -- Discontinuous time detected, send fault.
         Self.Fault_T_Send (Self.Faults.Discontinuous_Time_Fault (Timestamp, (Value => Seconds)));
      end if;

      -- Save off the last seconds value received:
      Self.Last_Received_Seconds := Seconds;
   end Tick_T_Recv_Sync;

end Component.Fault_Component.Implementation;
