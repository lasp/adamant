-- Turning off warnings for this import. This is in the unit test directory, and should
-- never be used for flight, so these warnings can be ignored.
pragma Warnings (Off, """System.Interrupt_Management.Operations"" is an internal GNAT unit");
pragma Warnings (Off, "use of this unit is non-portable and version-dependent");
with System.Interrupt_Management.Operations;
pragma Warnings (On, """System.Interrupt_Management.Operations"" is an internal GNAT unit");
pragma Warnings (On, "use of this unit is non-portable and version-dependent");

package body Interrupt_Sender is

   procedure Generate_Interrupt (Interrupt : in Ada.Interrupts.Interrupt_ID) is
   begin
      System.Interrupt_Management.Operations.Interrupt_Self_Process (System.Interrupt_Management.Interrupt_ID (Interrupt));
   end Generate_Interrupt;

end Interrupt_Sender;
