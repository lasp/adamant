--------------------------------------------------------------------------------
-- Crc_16 Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Crc_16;
with Basic_Types;
with String_Util;
with Ada.Text_IO; use Ada.Text_IO;

package body Crc_16_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Crc (Self : in out Instance) is
      use Crc_16;
      use Basic_Types;
      Ignore : Instance renames Self;
      Bytes : constant Basic_Types.Byte_Array (0 .. 14) := [16#06#, 16#00#, 16#0c#, 16#f0#, 16#00#, 16#04#, 16#00#, 16#55#, 16#88#, 16#73#, 16#c9#, 16#00#, 16#00#, 16#05#, 16#21#];
      Result : constant Crc_16_Type := Compute_Crc_16 (Bytes);
      Expected : constant Crc_16_Type := [0 => 16#75#, 1 => 16#fb#];
   begin
      Put_Line ("Returned CRC: " & String_Util.Bytes_To_String (Result));
      Put_Line ("Expected CRC: " & String_Util.Bytes_To_String (Expected));
      Assert (Result = Expected, "Test CRC failed!");
   end Test_Crc;

   overriding procedure Test_Crc_Seeded (Self : in out Instance) is
      use Crc_16;
      use Basic_Types;
      Ignore : Instance renames Self;
      Bytes : constant Basic_Types.Byte_Array (0 .. 14) := [16#06#, 16#00#, 16#0c#, 16#f0#, 16#00#, 16#04#, 16#00#, 16#55#, 16#88#, 16#73#, 16#c9#, 16#00#, 16#00#, 16#05#, 16#21#];
      Result1 : constant Crc_16_Type := Compute_Crc_16 (Bytes (0 .. 0));
      Result2 : constant Crc_16_Type := Compute_Crc_16 (Bytes (1 .. 1), Result1);
      Result3 : constant Crc_16_Type := Compute_Crc_16 (Bytes (2 .. 6), Result2);
      Result : constant Crc_16_Type := Compute_Crc_16 (Bytes (7 .. 14), Result3);
      Expected : constant Crc_16_Type := [0 => 16#75#, 1 => 16#fb#];
   begin
      Put_Line ("Returned CRC: " & String_Util.Bytes_To_String (Result));
      Put_Line ("Expected CRC: " & String_Util.Bytes_To_String (Expected));
      Assert (Result = Expected, "Test CRC failed!");
   end Test_Crc_Seeded;

end Crc_16_Tests.Implementation;
