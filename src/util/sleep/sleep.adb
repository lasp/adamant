
package body Sleep is

   procedure Sleep (Duration : in Ada.Real_Time.Time_Span) is
      use Ada.Real_Time;
      Wake_Up_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Duration;
   begin
      delay until Wake_Up_Time;
   end Sleep;

   procedure Sleep_Ms (Millisecs : in Natural) is
      use Ada.Real_Time;
      Delay_Time_Span : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (Millisecs);
      Wake_Up_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Delay_Time_Span;
   begin
      delay until Wake_Up_Time;
   end Sleep_Ms;

   procedure Sleep_Us (Microsecs : in Natural) is
      use Ada.Real_Time;
      Delay_Time_Span : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Microsecs);
      Wake_Up_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Delay_Time_Span;
   begin
      delay until Wake_Up_Time;
   end Sleep_Us;

end Sleep;
