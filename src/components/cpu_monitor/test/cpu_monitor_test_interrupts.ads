with Interrupt_Types;

-- Target-selected interrupt list for the CPU monitor test. The Linux
-- body monitors two POSIX signals; the bareboard runtime has no POSIX
-- signal names, so its body monitors no interrupts and the test covers
-- task usage only there.
package Cpu_Monitor_Test_Interrupts is

   function Get return Interrupt_Types.Interrupt_Id_List_Access;

end Cpu_Monitor_Test_Interrupts;
