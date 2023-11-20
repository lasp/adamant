with Ada.Interrupts;
with Ada.Execution_Time;

-- Package which hides Ada.Execution_Time.Interrupts which isn't
-- available on all systems, notably Linux native. If the function
-- is not implemented, then Zero time is returned.
package Interrupt_Cpu_Usage is

   -- Same as Ada.Execution_Time.Interrupts.Clock:
   function Interrupt_Clock (Interrupt : in Ada.Interrupts.Interrupt_ID) return Ada.Execution_Time.CPU_Time with
      Inline => True;

end Interrupt_Cpu_Usage;
