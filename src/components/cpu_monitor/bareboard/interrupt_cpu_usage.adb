with Ada.Execution_Time.Interrupts;

package body Interrupt_Cpu_Usage is

   function Interrupt_Clock (Interrupt : in Ada.Interrupts.Interrupt_ID) return Ada.Execution_Time.CPU_Time is
   begin
      return Ada.Execution_Time.Interrupts.Clock (Interrupt);
   end Interrupt_Clock;

end Interrupt_Cpu_Usage;
