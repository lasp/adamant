with Interfaces;

-- An example of a custom interrupt handler for unit testing purposes:
package body Tester_Interrupt_Handler is

   procedure Handler (Data : in out Tick.T) is
      use Interfaces;
   begin
      -- Increment the count:
      Data.Count := @ + 1;
      -- Increment the time:
      Data.Time := (0, 0);
   end Handler;

end Tester_Interrupt_Handler;
