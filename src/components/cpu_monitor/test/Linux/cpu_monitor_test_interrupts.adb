with Ada.Interrupts.Names;

package body Cpu_Monitor_Test_Interrupts is

   List : aliased Interrupt_Types.Interrupt_Id_List :=
      [0 => Ada.Interrupts.Names.SIGUSR1, 1 => Ada.Interrupts.Names.SIGUSR2];

   function Get return Interrupt_Types.Interrupt_Id_List_Access is (List'Access);

end Cpu_Monitor_Test_Interrupts;
