with Interfaces;

package body Tick_Interrupt_Handler is

   procedure Handler (Data : in out Tick.T) is
      use Interfaces;
   begin
      -- Increment the count:
      Data.Count := @ + 1;
   end Handler;

   procedure Set_Tick_Time (Data : in out Tick.T; Time : in Sys_Time.T) is
   begin
      -- Set the time field:
      Data.Time := Time;
   end Set_Tick_Time;

end Tick_Interrupt_Handler;
