-- Arithmetic package for Sys_Time
-- See a-reatim.ads and adb for detail on how math is done post conversion
-- Notes from a-reatim.ads:
--   Time and Time_Span are represented in 64-bit Duration value in
--   nanoseconds. For example, 1 second and 1 nanosecond is represented
--   as the stored integer 1_000_000_001. This is for the 64-bit Duration
--   case, not clear if this also is used for 32-bit Duration values.

package body Sys_Time.Arithmetic is

   -- Convert an Ada.Real_Time.Time to a Sys_Time
   -- This can result in an under_flow if the Time sent is negative. In that case an under_flow status will be sent and with a time of zero which is the earliest time available
   function To_Sys_Time (Arg_In : in Time; Arg_Out : out Sys_Time.T) return Sys_Time_Status is
      Seconds_Since_Epoch : Seconds_Count;
      Sub_Second_Time_Span : Time_Span;

      -- largest seconds value a Sys_Time can hold
      Max_Seconds : constant Seconds_Count := Seconds_Count (Unsigned_32'Last);
      Time_Zero : constant Time := Time_Of (Seconds_Count (0), Nanoseconds (0));

      -- 2^32 / one_second (in nanoseconds as represented in a time_span) / Duration'Small
      -- or 2^32
      Conversion_Factor : constant Duration := 10#4.2949_6729_6#E+9;
   begin
      -- Split the time into seconds and sub seconds:
      Split (Arg_In, Seconds_Since_Epoch, Sub_Second_Time_Span);

      -- Check if the Time is negative, if it is system time cannot handle it
      if Arg_In < Time_Zero then
         -- Set arg_out to zero
         Arg_Out := Sys_Time_Zero;
         -- We can not handle this time, it is negative
         return Underflow;
      elsif Seconds_Since_Epoch > Max_Seconds then
         -- Set arg_out to largest Sys_Time possible
         Arg_Out := Sys_Time_Max;
         -- We can not handle this time, it is larger than Unsigned_32'Last
         return Overflow;
      end if;

      -- Complete the conversion
      -- Set the seconds for the system time, this assumes that the Ada real time has the same Epoch:
      Arg_Out.Seconds := Unsigned_32 (Seconds_Since_Epoch);

      -- Convert from a subsecond Time_Span to Sys_Time subseconds
      Arg_Out.Subseconds := Unsigned_32 (To_Duration (Sub_Second_Time_Span) * Conversion_Factor);

      return Success;
   exception
      when Constraint_Error => return Underflow;
   end To_Sys_Time;

   -- Convert a Sys_Time to an Ada.Real_Time.Time
   function To_Time (Arg : in Sys_Time.T) return Time is
      To_Return : Time;
      Seconds_Since_Epoch : Seconds_Count;
      Sub_Second_Time_Span : Time_Span;

      -- This is the conversion factor between sub-seconds and nanoseconds * 10E9, it is converted to a Unsigned_64 so that
      -- It is possible to do integer math
      Subsec_To_Nsec : constant Unsigned_64 := 2_328_306_436;

      -- Check for Unsigned_64 overflow
      pragma Compile_Time_Error (Subsec_To_Nsec * 2**32 > Unsigned_64'Last, "This defines the conversion factor between sub-seconds and nanoseconds, times 10E10, if this is not less than Long_Integer, then need to switch to Long_Long_Integer");
   begin
      -- Used to check the precision of Real_Time.Time
      -- Put_Line("Duration'Size = " & Integer'Image(Duration'Size));
      -- Put_Line("Float: Duration'Small = " & Float'Image(Duration'Small));
      -- Put_Line("Duration: Duration'Small = " & Duration'Image(Duration'Small));
      -- Put_Line("Duration'First = " & Duration'Image(Duration'First));
      -- Put_Line("Duration'Last = " & Duration'Image(Duration'Last));
      -- New_Line;
      -- Put_Line("Integer'Last: " & Integer'Image(Integer'Last));
      -- Put_Line("Unsigned_32'Last: " & Unsigned_32'Image(Unsigned_32'Last));
      -- Put_Line("Long_Integer'Last: " & Long_Integer'Image(Long_Integer'Last));
      -- Put_Line("Duration, one second: " & Duration'Image(To_Duration(Seconds(1))) );
      -- New_Line;

      -- Convert seconds to seconds_count. Seconds count is implemented as a 64-bit number
      -- and so this should always be a safe conversion.
      Seconds_Since_Epoch := Seconds_Count (Arg.Seconds);

      -- Convert system time sub-seconds to a Time_Span by:
      -- Converting the sub-seconds to nanoseconds by multiplying by the conversion factor and then dividing by 10^9
      Sub_Second_Time_Span := Nanoseconds (Integer ((Unsigned_64 (Arg.Subseconds) * Subsec_To_Nsec) / 10#10#E9));

      -- Convert to a time using Ada Time_Of function
      To_Return := Time_Of (Seconds_Since_Epoch, Sub_Second_Time_Span);
      return To_Return;
   end To_Time;

   -- Add a system time and a time span and return a system time
   function Add (Left : in Sys_Time.T; Right : in Time_Span; Result : out Sys_Time.T) return Sys_Time_Status is
      Time_Left : constant Time := To_Time (Left);
      Time_Result : Time;

      -- return status of To_Sys_Time
      Status : Sys_Time_Status;
   begin
      Time_Result := Time_Left + Right;

      -- No over or under flow
      Status := To_Sys_Time (Time_Result, Result);
      return Status;
   exception
      when Constraint_Error => return Overflow;
   end Add;

   -- Add a system time and a time span and return a system time
   function Add (Left : in Time_Span; Right : in Sys_Time.T; Result : out Sys_Time.T) return Sys_Time_Status is
      Time_Right : constant Time := To_Time (Right);
      Time_Result : Time;

      -- return status of To_Sys_Time
      Status : Sys_Time_Status;
   begin
      Time_Result := Time_Right + Left;

      Status := To_Sys_Time (Time_Result, Result);
      return Status;
   exception
      when Constraint_Error => return Overflow;
   end Add;

   -- Subtract one time from another and return a time span, or subtract a time_span from a time and return a Subtract_Time_Span_Status
   -- Result is returned in Result : Sys_Time.T because subtracting a Time_Span from a Sys_Time could result in a Underflow meaning a time less than 0
   -- This is not representable in a Sys_Time so a status must be returned and check when using this function
   function Subtract (Left : Sys_Time.T; Right : Time_Span; Result : out Sys_Time.T) return Sys_Time_Status is
      Time_Left : constant Time := To_Time (Left);

      -- return status of To_Sys_Time
      Status : Sys_Time_Status;
   begin
      Status := To_Sys_Time (Time_Left - Right, Result);
      return Status;
   exception
      when Constraint_Error => return Underflow;
   end Subtract;

   -- Subtract one time from another and return a time span, or subtract a time_span from a time and return a system time
   -- Right is always subtracted from left
   function "-" (Left : Sys_Time.T; Right : Sys_Time.T) return Time_Span is
      Time_Left : constant Time := To_Time (Left);
      Time_Right : constant Time := To_Time (Right);
      Span_Return : Time_Span;
   begin
      Span_Return := Time_Left - Time_Right;
      return Span_Return;
   end "-";

   -- Less than check
   function "<" (Left, Right : Sys_Time.T) return Boolean is
   begin
      return (To_Time (Left) < To_Time (Right));
   end "<";

   -- Less than or equal to check
   function "<=" (Left, Right : Sys_Time.T) return Boolean is
   begin
      return (To_Time (Left) <= To_Time (Right));
   end "<=";

   -- Greater than check
   function ">" (Left, Right : Sys_Time.T) return Boolean is
   begin
      return (To_Time (Left) > To_Time (Right));
   end ">";

   -- Greater than or equal to check
   function ">=" (Left, Right : Sys_Time.T) return Boolean is
   begin
      return (To_Time (Left) >= To_Time (Right));
   end ">=";

end Sys_Time.Arithmetic;
