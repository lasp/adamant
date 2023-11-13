-- The Ada.Execution_Time.Interrupts package is not implemented for Linux,
-- so just return Cpu_Time_First when asked.
package body Interrupt_Cpu_Usage is

   function Interrupt_Clock (Interrupt : in Ada.Interrupts.Interrupt_ID) return Ada.Execution_Time.CPU_Time is
      Ignore : Ada.Interrupts.Interrupt_ID renames Interrupt;
   begin
      return Ada.Execution_Time.Time_Of (0);
   end Interrupt_Clock;

end Interrupt_Cpu_Usage;
