package body Cpu_Monitor_Test_Interrupts is

   --  Empty because there is no hook to uniformly stimulate interrupts on
   --  all bareboard targets, so we only test that feature on Linux for unit
   --  level testing.
   List : aliased Interrupt_Types.Interrupt_Id_List := [];

   function Get return Interrupt_Types.Interrupt_Id_List_Access is (List'Access);

end Cpu_Monitor_Test_Interrupts;
