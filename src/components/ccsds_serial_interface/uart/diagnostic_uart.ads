with Basic_Types;

--
-- Many GNAT bare board implementations hook Ada.Text_IO to a diagnostic UART.
-- This module uses Ada.Text_IO under the hood but allows raw byte array communication
-- over the diagnostic UART. The default implementation included in the default/
-- subdirectory can be used to provide a quick and dirty (non-flight)
-- backdoor for commanding and telemetry.
--
-- Note that when using the default implementation of this package,
-- a Put_Line somewhere else in the system will conflict with output of this
-- package, since both will send data out of the same serial port. In other
-- words, DO NOT use Text_IO and this package concurrently, or bad things
-- will happen.
--
-- If you do not want to use the default implementation, you can write your
-- own implementation of diagnostic_uart.adb and remove the default implementation
-- from the build path via:
--
-- $ export REMOVE_BUILD_PATH=~/adamant/src/components/ccsds_serial_interface/uart
--
-- and make sure your implementation is included instead.
--

package Diagnostic_Uart is

   -- Get a single byte from the serial port. This function
   -- will block until Rx is ready, so check first if needed.
   function Get return Basic_Types.Byte
      with Inline => True;

   -- Receive multiple bytes over the serial port. The byte array
   -- will be filled completely before this function returns. It will
   -- block until all bytes are filled.
   procedure Get (Bytes : out Basic_Types.Byte_Array)
      with Inline => True;

   -- Send a single byte to the serial port. This function
   -- will block until Tx is ready, so check first if needed.
   procedure Put (B : in Basic_Types.Byte)
      with Inline => True;

   -- Send multiple bytes over the serial port:
   procedure Put (Bytes : in Basic_Types.Byte_Array)
      with Inline => True;

end Diagnostic_Uart;
