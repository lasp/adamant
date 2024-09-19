--------------------------------------------------------------------------------
-- Ticker Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Ticker.Implementation is

   ---------------------------------------
   -- Cycle function:
   ---------------------------------------
   overriding procedure Cycle (Self : in out Instance) is
   begin
      -- Set the next period to the current clock just the first iteration:
      if Self.First then
         Self.First := False;
         Self.Next_Period := Ada.Real_Time.Clock;
      end if;

      -- Delay until the next period:
      delay until Self.Next_Period;

      -- Send the tick and update the count:
      Self.Tick_T_Send ((Time => Self.Sys_Time_T_Get, Count => Self.Count));
      Self.Count := @ + 1;

      -- Calculate the next wake-up time:
      Self.Next_Period := @ + Self.Period;
   end Cycle;

end Component.Ticker.Implementation;
