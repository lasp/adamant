with Sys_Time;
with Tick;

--
-- Because the Tick.T type is such a common type used to parameterize the
-- interrupt handler components (i.e. the Interrupt Servicer) this package contains
-- common code used to instantiate a Tick.T based interrupt component.
--
package Tick_Interrupt_Handler is

   -- A simple interrupt handler that increments the count in the Tick.T type:
   procedure Handler (Data : in out Tick.T);

   -- A procedure that sets the Sys_Time.T field in the Tick.T type. This procedure is
   -- useful for instantiating the Set_Interrupt_Data_Time formal parameter in the interrupt
   -- handler components.
   procedure Set_Tick_Time (Data : in out Tick.T; Time : in Sys_Time.T);

end Tick_Interrupt_Handler;
