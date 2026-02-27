--------------------------------------------------------------------------------
-- Sys_Time Tests Body
--------------------------------------------------------------------------------

with Sys_Time.Assertion; use Sys_Time.Assertion;
with Sys_Time.Arithmetic; use Sys_Time.Arithmetic;
with Signed_Delta_Time.Arithmetic;
with Signed_Delta_Time.Representation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Interfaces; use Interfaces;

package body Sys_Time_Tests.Implementation is
   --
   Tolerance : constant Time_Span := Nanoseconds (60_000);

   -- Generator used to create random floats
   G : Generator;

   Print_Flag1 : Boolean := True;

   Print_Flag2 : Boolean := True;

   -- return a Long_Float that is consistent with a duration range
   function Random_Long_Float_Duration return Long_Float is

      -- This is 2^32 in seconds with a very large subseconds value
      -- divided by 2 because we don't want to roll over when adding
      Duration_Largest : constant Long_Float := 4_294_967_296.99999999999999; --9223372036.854775807/2.1;

   begin

      return Long_Float (Random (G)) * Duration_Largest;
   end Random_Long_Float_Duration;

   -- return a system time given a Long_Float that is consistent with a duration range
   function Long_Float_To_Sys_Time (Arg_In : in Long_Float) return Sys_Time.T is
      -- return status of To_Sys_Time
      Status : Sys_Time_Status;
      To_Return : Sys_Time.T;

   begin
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), To_Time_Span (Duration (abs (Arg_In)))), To_Return);
      if Status = Overflow and then Print_Flag1 then
         Put_Line ("Long_Float_to_Sys_Time returned an Overflow status, input Long_Float was outside valid range:" & Long_Float'Image (Arg_In));
         Put_Line ("This is expected for some conversions and is part of the test");
         New_Line;
         Print_Flag1 := False;
      end if;
      return To_Return;
   end Long_Float_To_Sys_Time;

   -- return a Time_Span given a Long_Float that is consistent with a duration range
   function Long_Float_To_Time_Span (Arg_In : in Long_Float) return Time_Span is
   begin

      return To_Time_Span (Duration (Arg_In));
   end Long_Float_To_Time_Span;

   -- generate a set of corresponding values (Sys_Time.T, Long_Float)
   procedure Gen_Rand_Time (Time_Float : out Long_Float; Time_Out : out Sys_Time.T) is

   begin
      Time_Float := Random_Long_Float_Duration;
      Time_Out := Long_Float_To_Sys_Time (Time_Float);
   end Gen_Rand_Time;

   -- generate a set of corresponding values (Ada.Real_Time.Time_Span, Long_Float)
   procedure Gen_Rand_Time_Span (Time_Float : out Long_Float; Time_Span_Out : out Time_Span) is

   begin
      Time_Float := Random_Long_Float_Duration;
      Time_Span_Out := Long_Float_To_Time_Span (Time_Float);
   end Gen_Rand_Time_Span;

   -- Run the specified number of random add tests
   procedure Test_Add_Rand_Time (Tests : in Integer := 500) is
      -- Used to print out floating point numbers
      package Flt_Io is new Ada.Text_Io.Float_Io (Float);
      use Flt_Io;

      Float1 : Long_Float;
      Float2 : Long_Float;
      Float_Sum : Long_Float;
      Float_Sum_Time : Sys_Time.T;

      -- Current random time
      Rand_Time : Sys_Time.T;
      -- Current random time_span
      Rand_Span : Time_Span;

      -- Sum of rand_time and rand_span
      Time_Sum : Sys_Time.T;

      Difference : Time_Span;
      Diff_Sum : Time_Span := Nanoseconds (0);
      Diff_Sum_Float : Float;
      Diff_Max : Time_Span := Nanoseconds (0);
      Diff_Ave : Float;

      -- Need a different eps for this because of the floating point conversions
      Eps : constant Time_Span := Microseconds (32);

      -- return status of Sys_Time arithmetic
      Status : Sys_Time_Status;

   begin
      for I in Integer range 1 .. Tests loop

         Gen_Rand_Time (Float1, Rand_Time);
         Gen_Rand_Time_Span (Float2, Rand_Span);

         Float_Sum := Float1 + Float2;
         Float_Sum_Time := Long_Float_To_Sys_Time (Float_Sum);

         Status := Add (Rand_Time, Rand_Span, Time_Sum);

         if Status /= Success then
            if Print_Flag2  then
               Put_Line ("random add span returned " & Sys_Time_Status'Image (Status));
               Put_Line ("This is expected for some additions and is part of the test");
               New_Line;
            end if;
            Print_Flag2 := False;
         else
            Difference := Float_Sum_Time - Time_Sum;

            -- Update sum
            Diff_Sum := Diff_Sum + abs (Difference);

            -- Keep track of max difference
            if abs (Difference) > Diff_Max then
               Diff_Max := Difference;
            end if;

            -- Put_Line ("Difference (nsec): " & Integer'Image( difference/Nanoseconds(1) ) & ",");

            -- diff_sum_float := Float(To_Duration(diff_sum));
            -- Put_Line (ASCII.LF & "Difference Sum(nsec): " & Float'Image( diff_sum_float ));

            Sys_Time_Assert.Eq (Float_Sum_Time, Time_Sum, Eps);
         end if;
      end loop;

      --Put_Line("tests: " & Integer'Image(tests));
      --Put_Line (ASCII.LF & "Difference Sum(nsec): " & Integer'Image( diff_sum/Nanoseconds(1) ) & ",");
      Diff_Sum_Float := Float (To_Duration (Diff_Sum));
      -- Put_Line (ASCII.LF & "Difference Sum(nsec): " & Float'Image( diff_sum_float ));
      Diff_Ave := (Diff_Sum_Float / Float (Tests)) * 10.0**9;
      Put_Line (ASCII.LF & "Add Time Random Tests" & ", Tests Run:" & Integer'Image (Tests));
      Put_Line ("   Difference Max(nsec):   " & Integer'Image (Diff_Max / Nanoseconds (1)));
      Put ("   Difference Ave(nsec):");
      Put (Diff_Ave, 5, 2, 0);
      New_Line (2);
   end Test_Add_Rand_Time;

   -- Run the specified number of random subtraction tests
   procedure Test_Subtract_Rand_Time (Tests : in Integer := 500) is
      -- Used to print out floating point numbers
      package Flt_Io is new Ada.Text_Io.Float_Io (Float);
      use Flt_Io;

      -- Used to the floating point math along side the Sys_Time math
      Float1 : Long_Float;
      Float2 : Long_Float;
      Float_Sum : Long_Float;
      Float_Sum_Time_Span : Time_Span;

      -- First random time
      Rand_Time1 : Sys_Time.T;
      -- Second random time
      Rand_Time2 : Sys_Time.T;

      -- Sum of rand_time1 and rand_time2
      Time_Sum : Time_Span;

      -- The difference between the floating point math and the Sys_Time math on the current calculation
      Difference : Time_Span;

      -- Sum of the differences over all tests
      Diff_Sum : Time_Span := Nanoseconds (0);
      Diff_Sum_Float : Float;

      -- Maximum difference over all tests
      Diff_Max : Time_Span := Nanoseconds (0);

      -- Average of differences
      Diff_Ave : Float;

      -- Need a different eps for this because the floating point conversions decrease the precision of the conversions
      Eps : constant Time_Span := Microseconds (32);

      -- return status of Sys_Time arithmetic
      Ignore : Sys_Time_Status;

      -- Sys_Times used for final comparison
      Sys_Time1 : Sys_Time.T;
      Sys_Time2 : Sys_Time.T;

   begin
      for I in Integer range 1 .. Tests loop

         Gen_Rand_Time (Float1, Rand_Time1);
         Gen_Rand_Time (Float2, Rand_Time2);

         Float_Sum := Float1 - Float2;
         Float_Sum_Time_Span := Long_Float_To_Time_Span (Float_Sum);

         Time_Sum := Rand_Time1 - Rand_Time2;

         Difference := Float_Sum_Time_Span - Time_Sum;

         -- Update sum
         Diff_Sum := Diff_Sum + abs (Difference);

         -- Keep track of max difference
         if abs (Difference) > Diff_Max then
            Diff_Max := Difference;
         end if;

         -- Used to output the differences between the float math and the time math
         -- Put_Line ("Difference (nsec): " & Integer'Image( difference/Nanoseconds(1) ) & ",");

         -- Used to monitor the sum of the differences as it grows
         -- diff_sum_float := Float(To_Duration(diff_sum));
         -- Put_Line (ASCII.LF & "Difference Sum(nsec): " & Float'Image( diff_sum_float ));

         -- System times can not be negative, so negative time spans must be skipped
         --if float_sum_time_span > nanoseconds(0) and time_sum > nanoseconds(0) then
         Ignore := To_Sys_Time (Time_Of (Seconds_Count (0), Float_Sum_Time_Span), Sys_Time1);
         Ignore := To_Sys_Time (Time_Of (Seconds_Count (0), Float_Sum_Time_Span), Sys_Time2);
         Sys_Time_Assert.Eq (Sys_Time1, Sys_Time2, Eps);
         --end if;
      end loop;

      --Put_Line("tests: " & Integer'Image(tests));
      --Put_Line (ASCII.LF & "Difference Sum(nsec): " & Integer'Image( diff_sum/Nanoseconds(1) ) & ",");
      Diff_Sum_Float := Float (To_Duration (Diff_Sum));
      -- Put_Line (ASCII.LF & "Difference Sum(nsec): " & Float'Image( diff_sum_float ));
      Diff_Ave := (Diff_Sum_Float / Float (Tests)) * 10.0**9;
      Put_Line (ASCII.LF & "Subtract Time Random Tests" & ", Tests Run:" & Integer'Image (Tests));
      Put_Line ("   Difference Max(nsec):   " & Integer'Image (Diff_Max / Nanoseconds (1)));
      Put ("   Difference Ave(nsec):");
      Put (Diff_Ave, 5, 2, 0);
      New_Line (2);
   end Test_Subtract_Rand_Time;

   -- Run the specified number of random subtraction using a time_span tests
   procedure Test_Subtract_Rand_Time_Span (Tests : in Integer := 500) is
      -- Used to print out floating point numbers
      package Flt_Io is new Ada.Text_Io.Float_Io (Float);
      use Flt_Io;

      -- Used to the floating point math along side the Sys_Time math
      Float1 : Long_Float;
      Float2 : Long_Float;
      Float_Sum : Long_Float;
      Float_Sum_Sys_Time : Sys_Time.T;

      -- Current random time
      Rand_Time : Sys_Time.T;
      -- Current random time_span
      Rand_Time_Span : Time_Span;

      -- Result from rand_time and rand_time_span
      Time_Diff : Sys_Time.T;
      Seen_Underflow : Boolean := False;

      -- The difference between the floating point math and the Sys_Time math on the current calculation
      Difference : Time_Span;

      -- Sum of the differences over all tests
      Diff_Sum : Time_Span := Nanoseconds (0);
      Diff_Sum_Float : Float;

      -- Maximum difference over all tests
      Diff_Max : Time_Span := Nanoseconds (0);

      -- Average of differences
      Diff_Ave : Float;

      -- Need a different eps for this because the floating point conversions decrease the precision of the conversions
      Eps : constant Time_Span := Microseconds (32);

      -- return status of Sys_Time arithmetic
      Status : Sys_Time_Status;

   begin
      for I in Integer range 1 .. Tests loop

         Gen_Rand_Time (Float1, Rand_Time);
         Gen_Rand_Time_Span (Float2, Rand_Time_Span);

         Float_Sum := Float1 - Float2;
         Float_Sum_Sys_Time := Long_Float_To_Sys_Time (Float_Sum);

         Status := Subtract (Rand_Time, Rand_Time_Span, Time_Diff);

         -- Check status of the Sys_Time arithmetic
         if Status /= Success then
            if not Seen_Underflow then
               Put_Line ("Subtraction status is " & Sys_Time_Status'Image (Status) & ", Random tests verified Underflow is handled gracefully");
               Seen_Underflow := True;
            end if;
         else
            -- Calculate the difference between the floating point math and the time math
            Difference := Float_Sum_Sys_Time - Time_Diff;

            -- Update sum
            Diff_Sum := Diff_Sum + Difference;

            -- Keep track of max difference
            if abs (Difference) > Diff_Max then
               Diff_Max := Difference;
            end if;

            -- -- Used to output the differences between the float math and the time math
            -- Put_Line ("Difference (nsec): " & Integer'Image( difference/Nanoseconds(1) ) & ",");

            -- -- Used to monitor the sum of the differences as it grows
            -- diff_sum_float := Float(To_Duration(diff_sum));
            -- Put_Line (ASCII.LF & "Difference Sum(nsec): " & Float'Image( diff_sum_float ));

            -- check equality
            Sys_Time_Assert.Eq (Float_Sum_Sys_Time, Time_Diff, Eps);
         end if; -- END IF negative result check
      end loop;

      --Put_Line("tests: " & Integer'Image(tests));
      --Put_Line (ASCII.LF & "Difference Sum(nsec): " & Integer'Image( diff_sum/Nanoseconds(1) ) & ",");
      Diff_Sum_Float := Float (To_Duration (Diff_Sum));
      -- Put_Line (ASCII.LF & "Difference Sum(nsec): " & Float'Image( diff_sum_float ));
      Diff_Ave := (Diff_Sum_Float / Float (Tests)) * 10.0**9;
      Put_Line (ASCII.LF & "Subtract Time Random Time Span Tests" & ", Tests Run:" & Integer'Image (Tests));
      Put_Line ("   Difference Max(nsec):   " & Integer'Image (Diff_Max / Nanoseconds (1)));
      Put ("   Difference Ave(nsec):");
      Put (Diff_Ave, 5, 2, 0);
      New_Line (2);
   end Test_Subtract_Rand_Time_Span;
   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------
   overriding procedure Set_Up_Test (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
   begin
      -- Reset random number generator
      Reset (G);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Compare_Time (Self : in out Instance) is

      Ignore : Instance renames Self;

      Time1 : constant Sys_Time.T := (Seconds => 1, Subseconds => 0);

      Sys_Time1 : constant Ada.Real_Time.Time := Time_Of (Seconds_Count (1), Milliseconds (0));

      Sys_To_Real1 : Ada.Real_Time.Time;
      Real_To_Sys1 : Sys_Time.T;
      Back_Sys1 : Sys_Time.T;

      Time1_6 : constant Sys_Time.T := (Seconds => 1, Subseconds => Subseconds_Type (Unsigned_64 (3_000_000_000) / (Unsigned_64 (Unsigned_32'Last) + 1)));

      Sys_To_Real1_6 : Ada.Real_Time.Time;

      Back_Sys1_6 : Sys_Time.T;

      -- return status of Sys_Time arithmetic
      Status : Sys_Time_Status;
      pragma Unreferenced (Status);

   begin

      Sys_To_Real1 := To_Time (Time1);

      Status := To_Sys_Time (Sys_Time1, Real_To_Sys1);

      Status := To_Sys_Time (Sys_To_Real1, Back_Sys1);

      Sys_Time_Assert.Eq (Time1, Real_To_Sys1, Tolerance);
      Sys_Time_Assert.Eq (Time1, Back_Sys1, Tolerance);

      -- Test with ~1.6 seconds
      Sys_To_Real1_6 := To_Time (Time1_6);

      --real_to_sys1_6 := To_Sys_Time(sys_time1_6);

      --Put_Line(Sys_Time.Representation.Image(real_to_sys1_6));
      Status := To_Sys_Time (Sys_To_Real1_6, Back_Sys1_6);

      --Sys_Time_Assert.Eq(time1_6, real_to_sys1_6, tolerance);
      Sys_Time_Assert.Eq (Time1_6, Back_Sys1_6, Tolerance);

   end Compare_Time;

   overriding procedure Add_Time (Self : in out Instance) is
      Ignore : Instance renames Self;

      -- return status of Sys_Time arithmetic
      Status : Sys_Time_Status;
      pragma Unreferenced (Status);

      -- Test 1: Add two numbers less than one
      Time_Under_One : Sys_Time.T;
      Ts_Under_One : constant Time_Span := Milliseconds (100);
      Ans_Under_One : Sys_Time.T;

      -- Test 2: Add two numbers less than one, where sum is greater than one
      Time_Over_One : Sys_Time.T;
      Ts_Over_One : constant Time_Span := Milliseconds (800);
      Ans_Over_One : Sys_Time.T;

      -- Test 3: Add a very large number and a very small time span
      Time_Large : Sys_Time.T;
      Ts_Small : constant Time_Span := Milliseconds (25);
      Ans_Large_Small : Sys_Time.T;

      -- Test 4: Add a large time and a large Time_Span
      Time_Large4 : Sys_Time.T;
      Ts_Large : constant Time_Span := Milliseconds (9_875);
      Ans_Large_Large : Sys_Time.T;

      -- Test 5: Add a very large time span to a very small time
      Time_Under_One5 : Sys_Time.T;
      Ts_Large5 : constant Time_Span := Milliseconds (900_875);
      Ans_Small_Large : Sys_Time.T;

      Add_Result : Sys_Time.T;
   begin

      -- Test 1: Add two numbers less than one
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (500)), Time_Under_One);
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (600)), Ans_Under_One);
      Status := Add (Time_Under_One, Ts_Under_One, Add_Result);
      Sys_Time_Assert.Eq (Add_Result, Ans_Under_One, Tolerance);

      -- Test 2: Add two numbers less than one, where sum is greater than one
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (500)), Time_Over_One);
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (1_300)), Ans_Over_One);
      Status := Add (Time_Over_One, Ts_Over_One, Add_Result);
      Sys_Time_Assert.Eq (Add_Result, Ans_Over_One, Tolerance);

      -- Test 3: Add a very large number and a very small time span
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (123)), Time_Large);
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (148)), Ans_Large_Small);
      Status := Add (Time_Large, Ts_Small, Add_Result);
      Sys_Time_Assert.Eq (Add_Result, Ans_Large_Small, Tolerance);

      -- Test 4: Add a large time and a large Time_Span
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (123)), Time_Large4);
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (9_998)), Ans_Large_Large);
      Status := Add (Time_Large4, Ts_Large, Add_Result);
      Sys_Time_Assert.Eq (Add_Result, Ans_Large_Large, Tolerance);

      -- Test 5: Add a very large time span to a very small time
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (122)), Time_Under_One5);
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (900_997)), Ans_Small_Large);
      Status := Add (Time_Under_One5, Ts_Large5, Add_Result);
      Sys_Time_Assert.Eq (Add_Result, Ans_Small_Large, Tolerance);

      -- Do 5000 addition tests using random times and Time_Spans
      Test_Add_Rand_Time (20_000);

   end Add_Time;

   overriding procedure Subtract_Time (Self : in out Instance) is
      Ignore : Instance renames Self;

      -- return status of Sys_Time arithmetic
      Status : Sys_Time_Status;
      pragma Unreferenced (Status);
      -- Subtract status
      Subtract_Status : Sys_Time_Status;
      Time_Diff : Sys_Time.T;

      -- Test 1: Subtract two numbers less than one
      Time_Under_One : Sys_Time.T;
      Ts_Under_One : constant Time_Span := Milliseconds (100);
      Ans_Under_One : Sys_Time.T;

      -- Test 2: Subtract a Time_Span from a time greater than one such that the result is less than one
      Time_Over_One : Sys_Time.T;
      Ts_Over_One : constant Time_Span := Milliseconds (800);
      Ans_Under_One2 : Sys_Time.T;

      -- Test 3: Subtract a very large number and a very small time span
      Time_Large : Sys_Time.T;
      Ts_Small : constant Time_Span := Milliseconds (25);
      Ans_Large_Small : Sys_Time.T;

      -- Test 4: Subtract a large time and a large Time_Span
      Time_Large4 : Sys_Time.T;
      Ts_Large : constant Time_Span := Milliseconds (9_875);
      Ans_Large_Large : Sys_Time.T;

      -- Test 5: Subtract a large time from a large time
      Time_Large5 : Sys_Time.T;
      Time_Large_Sub : Sys_Time.T;
      Ans_Small_Large : Sys_Time.T;

      Sub_Result : Sys_Time.T;
   begin

      -- Test 1: Subtract two numbers less than one
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (500)), Time_Under_One);
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (400)), Ans_Under_One);
      Subtract_Status := Subtract (Time_Under_One, Ts_Under_One, Time_Diff);
      -- Check if time_diff is Null, if it is we have a negative Sys_Time as a result of the subtraction
      if Subtract_Status = Underflow then
         Put_Line ("Subtraction result is Underflow, negative result tested");
      else
         Sys_Time_Assert.Eq (Time_Diff, Ans_Under_One, Tolerance);
      end if;

      -- Test 2: Add two numbers less than one, where sum is greater than one
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (1_500)), Time_Over_One);
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (700)), Ans_Under_One2);
      Subtract_Status := Subtract (Time_Over_One, Ts_Over_One, Time_Diff);
      -- Check if time_diff is Null, if it is we have a negative Sys_Time as a result of the subtraction
      if Subtract_Status = Underflow then
         Put_Line ("Subtraction result is Underflow, negative result tested");
      else
         Sys_Time_Assert.Eq (Time_Diff, Ans_Under_One2, Tolerance);
      end if;

      -- Test 3: Add a very large number and a very small time span
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (123)), Time_Large);
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (98)), Ans_Large_Small);
      Subtract_Status := Subtract (Time_Large, Ts_Small, Time_Diff);
      -- Check if time_diff is Null, if it is we have a negative Sys_Time as a result of the subtraction
      if Subtract_Status = Underflow then
         Put_Line ("Subtraction result is Underflow, negative result tested");
      else
         Sys_Time_Assert.Eq (Time_Diff, Ans_Large_Small, Tolerance);
      end if;

      -- Test 4: Add a large time and a large Time_Span
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (123)), Time_Large4);
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (-9_752)), Ans_Large_Large);
      Subtract_Status := Subtract (Time_Large4, Ts_Large, Time_Diff);
      -- Check if time_diff is Null, if it is we have a negative Sys_Time as a result of the subtraction
      if Subtract_Status = Underflow then
         Put_Line ("Subtraction result is Underflow, negative result tested");
      else
         Sys_Time_Assert.Eq (Time_Diff, Ans_Large_Large, Tolerance);
      end if;

      -- Test 5: Add a very large time span to a very small time
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (123)), Time_Large5);
      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Milliseconds (123)), Ans_Small_Large);
      Status := To_Sys_Time (Time_Of (Seconds_Count (123_456_789), Milliseconds (0)), Time_Large_Sub);

      Status := To_Sys_Time (Time_Of (Seconds_Count (0), Time_Large5 - Time_Large_Sub), Sub_Result);
      Sys_Time_Assert.Eq (Sub_Result, Ans_Small_Large, Tolerance);

      -- Do 5000 addition tests using random times and Time_Spans
      Test_Subtract_Rand_Time (20_000);

      Test_Subtract_Rand_Time_Span (20_000);
   end Subtract_Time;

   -- This unit test adds some additional testing.
   overriding procedure Additional_Tests (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Time_1 : Sys_Time.T := (1167846707, Subseconds_Type (Unsigned_64 (1484907633) / (Unsigned_64 (Unsigned_32'Last) + 1))); -- Broadcast time
      Time_2 : Sys_Time.T := (1167846707, Subseconds_Type (Unsigned_64 (1485043675) / (Unsigned_64 (Unsigned_32'Last) + 1))); -- Broadcast recv time
      Time_Delta : Time_Span;
      Status : Sys_Time.Arithmetic.Sys_Time_Status;
      Time_Correction : Signed_Delta_Time.T;

      -- This test below reveals an issue if we do not handle the case where
      -- fixed point duration is rounded UP while converting to an unsigned
      -- integer, causing the subseconds to overflow.
      procedure Test_Subtract_Problematic is
         -- Error output.
         --
         --  Difference (nsec):  1000000000,
         --  Difference Sum(nsec):  9.99649E-01
         --
         --  Float
         --   4.29496268900000E+09
         --  Time:
         --  (SECONDS =>  4294962688,
         --   SUBSECONDS =>  0)
         --  ^ These are off by a whole second!
         --
         Float1 : constant Long_Float := 4.294962688999999E+09;
         Rand_Time : Sys_Time.T;
      begin
         Put_Line ("");
         Put_Line ("Begin Test_Subtract_Problematic");
         Rand_Time := Long_Float_To_Sys_Time (Float1);
         Put_Line ("input float:  4.294962688999999E+09;");
         Put_Line ("float image: " & Float1'Image);
         Put_Line ("Rand_Time: " & Rand_Time'Image);
         Put_Line ("End Test_Subtract_Problematic");
         Sys_Time_Assert.Eq (Rand_Time, (4294962689, 0), Eps => Microseconds (32));
      end Test_Subtract_Problematic;
   begin
      -- Get our final time delta
      Time_Delta := Time_1 - Time_2;
      -- Convert our time span to a signed delta time
      Status := Signed_Delta_Time.Arithmetic.To_Signed_Delta_Time (Time_Delta, Time_Correction);
      Put_Line (Sys_Time.Arithmetic.Sys_Time_Status'Image (Status));
      Put_Line (Signed_Delta_Time.Representation.Image (Time_Correction));

      -- Get our final time delta
      Time_Delta := Time_2 - Time_1;
      -- Convert our time span to a signed delta time
      Status := Signed_Delta_Time.Arithmetic.To_Signed_Delta_Time (Time_Delta, Time_Correction);
      Put_Line (Sys_Time.Arithmetic.Sys_Time_Status'Image (Status));
      Put_Line (Signed_Delta_Time.Representation.Image (Time_Correction));

      Time_1 := (1167846708, Subseconds_Type (Unsigned_64 (1484877568) / (Unsigned_64 (Unsigned_32'Last) + 1))); -- Broadcast time
      Time_2 := (1167846709, Subseconds_Type (Unsigned_64 (1484600950) / (Unsigned_64 (Unsigned_32'Last) + 1))); -- Broadcast recv time

      -- Get our final time delta
      Time_Delta := Time_1 - Time_2;
      -- Convert our time span to a signed delta time
      Status := Signed_Delta_Time.Arithmetic.To_Signed_Delta_Time (Time_Delta, Time_Correction);
      Put_Line (Sys_Time.Arithmetic.Sys_Time_Status'Image (Status));
      Put_Line (Signed_Delta_Time.Representation.Image (Time_Correction));

      -- Get our final time delta
      Time_Delta := Time_2 - Time_1;
      -- Convert our time span to a signed delta time
      Status := Signed_Delta_Time.Arithmetic.To_Signed_Delta_Time (Time_Delta, Time_Correction);
      Put_Line (Sys_Time.Arithmetic.Sys_Time_Status'Image (Status));
      Put_Line (Signed_Delta_Time.Representation.Image (Time_Correction));

      Time_1 := (1167846706, Subseconds_Type (Unsigned_64 (1484941993) / (Unsigned_64 (Unsigned_32'Last) + 1))); -- Broadcast time
      Time_2 := (1167846706, Subseconds_Type (Unsigned_64 (1484980626) / (Unsigned_64 (Unsigned_32'Last) + 1))); -- Broadcast recv time

      -- Get our final time delta
      Time_Delta := Time_1 - Time_2;
      -- Convert our time span to a signed delta time
      Status := Signed_Delta_Time.Arithmetic.To_Signed_Delta_Time (Time_Delta, Time_Correction);
      Put_Line (Sys_Time.Arithmetic.Sys_Time_Status'Image (Status));
      Put_Line (Signed_Delta_Time.Representation.Image (Time_Correction));

      -- Get our final time delta
      Time_Delta := Time_2 - Time_1;
      -- Convert our time span to a signed delta time
      Status := Signed_Delta_Time.Arithmetic.To_Signed_Delta_Time (Time_Delta, Time_Correction);
      Put_Line (Sys_Time.Arithmetic.Sys_Time_Status'Image (Status));
      Put_Line (Signed_Delta_Time.Representation.Image (Time_Correction));

      -- Run additional test:
      Test_Subtract_Problematic;
   end Additional_Tests;

end Sys_Time_Tests.Implementation;
