--------------------------------------------------------------------------------
-- Byte_Array_Util Tests Body
--------------------------------------------------------------------------------

with Byte_Array_Util;
with Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;

package body Byte_Array_Util_Tests.Implementation is

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

   -- Smart assertion packages:
   package Extract_Status_Assert is new Smart_Assert.Discrete (Byte_Array_Util.Extract_Poly_Type_Status, Byte_Array_Util.Extract_Poly_Type_Status'Image);
   package Set_Status_Assert is new Smart_Assert.Discrete (Byte_Array_Util.Set_Poly_Type_Status, Byte_Array_Util.Set_Poly_Type_Status'Image);

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Extract (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Byte_Array_Util;
      Val : Basic_Types.Poly_32_Type;
   begin
      -- Test extractions that end on byte boundaries:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [1, 2, 3, 4, 5, 6, 7], Offset => 0, Size => 32, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [1, 2, 3, 4]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [1, 2, 3, 4, 5, 6, 7], Offset => 8, Size => 32, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [2, 3, 4, 5]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [1, 2, 3, 4, 5, 6, 7], Offset => 8, Size => 24, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 2, 3, 4]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [1, 2, 3, 4, 5, 6, 7], Offset => 16, Size => 16, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 3, 4]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [1, 2, 3, 4, 5, 6, 7], Offset => 32, Size => 8, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 5]);

      -- Test extractions that do not end on byte boundaries:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 255], Offset => 0, Size => 1, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 1]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 255, 1 => 255], Offset => 8, Size => 1, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 1]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 255, 1 => 255], Offset => 5, Size => 5, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 16#1F#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 255, 1 => 255, 2 => 255, 3 => 255], Offset => 11, Size => 12, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 16#0F#, 16#FF#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 233, 1 => 255, 2 => 235, 3 => 255], Offset => 10, Size => 12, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 16#0F#, 16#FA#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 4, Size => 8, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 16#00#, 16#5A#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 4, Size => 16, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 16#5A#, 16#A5#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 3, Size => 16, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 16#AD#, 16#52#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 2, Size => 17, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 16#AD#, 16#52#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 3, Size => 17, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 1, 16#5A#, 16#A5#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 0, Size => 9, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 16#AB#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#2#], Offset => 6, Size => 2, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 16#2#]);

      -- Test illegal extractions:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 0, Size => 33, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 0, Size => 34, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 0, Size => 99, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 1, Size => 32, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 5, Size => 29, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#, 3 => 16#AA#], Offset => 32, Size => 1, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#, 2 => 16#55#], Offset => 24, Size => 1, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#], Offset => 14, Size => 3, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#], Offset => 16, Size => 1, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 16#55#, 1 => 16#AA#], Offset => 0, Size => 17, Is_Signed => False, Value => Val), Error);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [1, 2, 3, 4, 5, 6, 7], Offset => 50, Size => 7, Is_Signed => False, Value => Val), Error);

      -- Some more point tests:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 2#00000100#], Offset => 4, Size => 2, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 1]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 2#00001100#], Offset => 4, Size => 2, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 3]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [0 => 2#00101100#], Offset => 0, Size => 4, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 2]);
   end Test_Extract;

   -- This test makes sure the set polytype function works as expected.
   overriding procedure Test_Set (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Byte_Array_Util;
      Bytes : Basic_Types.Byte_Array := [2 .. 11 => 0];
   begin
      -- Test extractions that end on byte boundaries:
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 0, Size => 32, Value => [1, 2, 3, 4]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 1, 1 => 2, 2 => 3, 3 => 4, 4 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 16, Size => 32, Value => [5, 6, 7, 8]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 1, 1 => 2, 2 => 5, 3 => 6, 4 => 7, 5 => 8, 6 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 8, Size => 32, Value => [255, 255, 255, 255]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 1, 1 => 255, 2 => 255, 3 => 255, 4 => 255, 5 => 8, 6 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 8, Size => 32, Value => [0, 0, 0, 12]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 1, 1 => 0, 2 => 0, 3 => 0, 4 => 12, 5 => 8, 6 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 8, Size => 16, Value => [0, 0, 33, 33]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 1, 1 => 33, 2 => 33, 3 => 0, 4 => 12, 5 => 8, 6 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 0, Size => 16, Value => [0, 0, 22, 0]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 22, 1 => 0, 2 => 33, 3 => 0, 4 => 12, 5 => 8, 6 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4 * 8, Size => 24, Value => [0, 3, 2, 1]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 22, 1 => 0, 2 => 33, 3 => 0, 4 => 3, 5 => 2, 6 => 1, 7 .. 9 => 0]);

      -- Test extractions that do not end on byte boundaries:
      Bytes := [others => 0];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 8, Value => [0, 0, 0, 255]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 16#0f#, 1 => 16#f0#, 2 .. 9 => 0]);
      Bytes := [others => 255];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 8, Value => [0, 0, 0, 0]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 16#f0#, 1 => 16#0f#, 2 .. 9 => 255]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 8, Value => [0, 0, 0, 16#44#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 16#f4#, 1 => 16#4f#, 2 .. 9 => 255]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 8, Value => [0, 0, 0, 16#11#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 16#f1#, 1 => 16#1f#, 2 .. 9 => 255]);
      Bytes := [others => 0];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 3, Size => 16, Value => [0, 0, 16#FF#, 16#FF#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 31, 1 => 16#ff#, 2 => 224, 3 .. 9 => 0]);
      Bytes := [others => 255];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 3, Size => 16, Value => [0, 0, 16#00#, 16#00#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 224, 1 => 16#00#, 2 => 31, 3 .. 9 => 255]);
      Bytes := [others => 255];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 3, Size => 17, Value => [0, 0, 16#00#, 16#00#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 224, 1 => 16#00#, 2 => 15, 3 .. 9 => 255]);
      Bytes := [others => 0];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 17, Value => [0, 0, 16#FF#, 16#FF#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 16#07#, 1 => 16#FF#, 2 => 16#F8#, 3 .. 9 => 0]);
      Bytes := [others => 0];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 17, Value => [0, 1, 16#FF#, 16#FF#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 16#0F#, 1 => 16#FF#, 2 => 16#F8#, 3 .. 9 => 0]);
      Bytes := [others => 255];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 4, Size => 1, Value => [0, 0, 16#0#, 16#0#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 247, 1 .. 9 => 255]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 1, Size => 2, Value => [0, 0, 16#0#, 16#0#]), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 151, 1 .. 9 => 255]);

      -- Test truncation logic:
      Bytes := [others => 0];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 0, Size => 8, Value => [0, 0, 1, 0]), Truncation_Error);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 3, Size => 8, Value => [0, 0, 1, 0]), Truncation_Error);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 3, Size => 8, Value => [0, 0, 1, 0], Truncation_Allowed => False), Truncation_Error);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 3, Size => 8, Value => [0, 0, 1, 0], Truncation_Allowed => True), Success);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 0, Size => 8, Value => [0, 0, 255, 254], Truncation_Allowed => False), Truncation_Error);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 0, Size => 8, Value => [0, 0, 255, 254], Truncation_Allowed => True), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 254, 1 .. 9 => 0]);
      Bytes := [others => 255];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes (3 .. 11), Offset => 2, Size => 13, Value => [0, 1, 0, 0], Truncation_Allowed => False), Truncation_Error);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes (3 .. 11), Offset => 2, Size => 13, Value => [0, 1, 0, 0], Truncation_Allowed => True), Success);
      Byte_Array_Assert.Eq (Bytes, [0 => 255, 1 => 192, 2 => 1, 3 .. 9 => 255]);

      -- Test illegal extractions:
      Bytes := [others => 0];
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes (1 .. 0), Offset => 2, Size => 13, Value => [255, 255, 255, 255], Truncation_Allowed => True), Error);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes (2 .. 2), Offset => 0, Size => 9, Value => [255, 255, 255, 255], Truncation_Allowed => True), Error);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes (2 .. 2), Offset => 8, Size => 1, Value => [255, 255, 255, 255], Truncation_Allowed => True), Error);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes (2 .. 2), Offset => 2, Size => 7, Value => [255, 255, 255, 255], Truncation_Allowed => True), Error);
      Byte_Array_Assert.Eq (Bytes, [0 .. 9 => 0]);
   end Test_Set;

   -- This test makes sure the set and extract polytype functions work together as expected.
   overriding procedure Test_Set_And_Extract (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Byte_Array_Util;
      Val : Basic_Types.Poly_32_Type;
      Bytes : Basic_Types.Byte_Array := [2 .. 11 => 0];
   begin
      Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => 0, Size => 32, Value => [1, 2, 3, 4]), Success);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => Bytes, Offset => 0, Size => 32, Is_Signed => False, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [1, 2, 3, 4]);

      for Idx in 1 .. 44 loop
         Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => Idx, Size => 32, Value => [1, 2, 3, 4]), Success);
         Extract_Status_Assert.Eq (Extract_Poly_Type (Src => Bytes, Offset => Idx, Size => 32, Is_Signed => False, Value => Val), Success);
         Byte_Array_Assert.Eq (Val, [1, 2, 3, 4]);
      end loop;

      Bytes := [2 .. 11 => 243];
      for Idx in 0 .. 44 loop
         Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => Idx, Size => 16, Value => [0, 0, 16#FE#, 16#EF#]), Success);
         Extract_Status_Assert.Eq (Extract_Poly_Type (Src => Bytes, Offset => Idx, Size => 16, Is_Signed => False, Value => Val), Success);
         Byte_Array_Assert.Eq (Val, [0, 0, 16#FE#, 16#EF#]);
      end loop;

      Bytes := [2 .. 11 => 4];
      for Idx in 0 .. 44 loop
         Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => Idx, Size => 1, Value => [0, 0, 0, 16#1#]), Success);
         Extract_Status_Assert.Eq (Extract_Poly_Type (Src => Bytes, Offset => Idx, Size => 1, Is_Signed => False, Value => Val), Success);
         Byte_Array_Assert.Eq (Val, [0, 0, 16#0#, 16#1#]);
      end loop;

      Bytes := [2 .. 11 => 0];
      for Idx in 0 .. 44 loop
         Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => Idx, Size => 17, Value => [0, 0, 16#45#, 16#11#]), Success);
         Extract_Status_Assert.Eq (Extract_Poly_Type (Src => Bytes, Offset => Idx, Size => 17, Is_Signed => False, Value => Val), Success);
         Byte_Array_Assert.Eq (Val, [0, 0, 16#45#, 16#11#]);
      end loop;

      Bytes := [2 .. 11 => 16#AA#];
      for Idx in 0 .. 44 loop
         Set_Status_Assert.Eq (Set_Poly_Type (Dest => Bytes, Offset => Idx, Size => 2, Value => [0, 0, 16#45#, 16#12#], Truncation_Allowed => True), Success);
         Extract_Status_Assert.Eq (Extract_Poly_Type (Src => Bytes, Offset => Idx, Size => 2, Is_Signed => False, Value => Val), Success);
         Byte_Array_Assert.Eq (Val, [0, 0, 16#0#, 16#2#]);
      end loop;
   end Test_Set_And_Extract;

   -- This test makes sure the extract polytype function works for signed numbers that are negative.
   overriding procedure Test_Extract_Signed (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      use Byte_Array_Util;
      Val : Basic_Types.Poly_32_Type;
   begin
      -- Basic tests:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 8, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 255]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 7, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 16#7F#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 9, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 255]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 10, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 255]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 17, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 254]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 18, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 252]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 8, Size => 7, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 255]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 8, Size => 6, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 255]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#00#, 16#FF#, 16#FF#, 2, 3, 4, 5, 6, 7], Offset => 8, Size => 1, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 255]);

      -- Test with actual unsigned integers:
      -- 100 stored at bit 4 for 8-bit signed integer:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#06#, 16#40#, 16#00#, 2, 3, 4, 5, 6, 7], Offset => 4, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 100]);
      -- -100 stored at bit 4 for 8-bit signed integer:
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#09#, 16#C0#, 16#00#, 2, 3, 4, 5, 6, 7], Offset => 4, Size => 8, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 255, 16#9C#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#29#, 16#C0#, 16#00#, 2, 3, 4, 5, 6, 7], Offset => 2, Size => 10, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 255, 16#FE#, 16#9C#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#29#, 16#C0#, 16#00#, 2, 3, 4, 5, 6, 7], Offset => 3, Size => 9, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 0, 0, 16#9C#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#29#, 16#C3#, 16#00#, 2, 3, 4, 5, 6, 7], Offset => 2, Size => 18, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [255, 16#FE#, 16#9C#, 16#30#]);
      Extract_Status_Assert.Eq (Extract_Poly_Type (Src => [16#29#, 16#C3#, 16#00#, 2, 3, 4, 5, 6, 7], Offset => 0, Size => 20, Is_Signed => True, Value => Val), Success);
      Byte_Array_Assert.Eq (Val, [0, 16#02#, 16#9C#, 16#30#]);
   end Test_Extract_Signed;

end Byte_Array_Util_Tests.Implementation;
