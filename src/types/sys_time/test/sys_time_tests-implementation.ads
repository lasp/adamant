--------------------------------------------------------------------------------
-- Sys_Time Tests Spec
--------------------------------------------------------------------------------

with Sys_Time; use Sys_Time;
with Ada.Real_Time; use Ada.Real_Time;

-- This is a unit test suite for testing the system time arithmetic package
package Sys_Time_Tests.Implementation is
   -- Test data and state:
   type Instance is new Sys_Time_Tests.Base_Instance with private;
private

   -- return a Long_Float that is consistent with a duration range
   function Random_Long_Float_Duration return Long_Float;

   -- return a system time given a Long_Float that is consistent with a duration range
   function Long_Float_To_Sys_Time (Arg_In : in Long_Float) return Sys_Time.T;

   -- return a Time_Span given a Long_Float that is consistent with a duration range
   function Long_Float_To_Time_Span (Arg_In : in Long_Float) return Time_Span;

   -- generate a set of corresponding values (Sys_Time.T, Long_Float)
   procedure Gen_Rand_Time (Time_Float : out Long_Float; Time_Out : out Sys_Time.T);

   -- generate a set of corresponding values (Ada.Real_Time.Time_Span, Long_Float)
   procedure Gen_Rand_Time_Span (Time_Float : out Long_Float; Time_Span_Out : out Time_Span);

   -- Run the specified number of random add tests
   procedure Test_Add_Rand_Time (Tests : in Integer := 500);

   -- Run the specified number of random subtraction tests
   procedure Test_Subtract_Rand_Time (Tests : in Integer := 500);

   -- Run the specified number of random subtraction using a time_span tests
   procedure Test_Subtract_Rand_Time_Span (Tests : in Integer := 500);

   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure that comparing two times produces the correct results.
   overriding procedure Compare_Time (Self : in out Instance);
   -- This unit test makes sure that adding two times produces the correct results.
   overriding procedure Add_Time (Self : in out Instance);
   -- This unit test makes sure that subtracting two times produces the correct results.
   overriding procedure Subtract_Time (Self : in out Instance);
   -- This unit test adds some additional testing.
   overriding procedure Additional_Tests (Self : in out Instance);

   -- Test data and state:
   type Instance is new Sys_Time_Tests.Base_Instance with record
      null;
   end record;
end Sys_Time_Tests.Implementation;
