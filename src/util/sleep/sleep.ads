with Ada.Real_Time;

package Sleep is

   -- Pause task for duration equal to the value of an
   -- Ada Time_Span type before resuming.
   procedure Sleep (Duration : in Ada.Real_Time.Time_Span);

   -- Pause task for certain number of milliseconds
   -- before resuming.
   procedure Sleep_Ms (Millisecs : in Natural);

   -- Pause task for certain number of microseconds
   -- before resuming.
   procedure Sleep_Us (Microsecs : in Natural);

end Sleep;
