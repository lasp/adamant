with Tick;

-- An example of a custom interrupt handler for unit testing purposes:
package Tester_Interrupt_Handler is

   procedure Handler (Data : in out Tick.T);

end Tester_Interrupt_Handler;
