with Ada.Text_IO;
with Diagnostic_Uart;

procedure Test is
begin

   -- Hello, world!
   Diagnostic_Uart.Put ([72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33]);

   --  Sentinel for the cross test runner.
   Ada.Text_IO.Put_Line ("=== ALL TESTS PASSED ===");

end Test;
