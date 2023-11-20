with Ada.Interrupts;

package Interrupt_Sender is

   -- Taken from http://www2.adacore.com/gap-static/GNAT_Book/html/rts/a-intsig__adb.htm
   -- a package that is not included in this version of Ada for some reason...
   -- but it allows us to send a signal as an interrupt on Linux.
   procedure Generate_Interrupt (Interrupt : in Ada.Interrupts.Interrupt_ID);

end Interrupt_Sender;
