package body Stopwatch.Representation is

   function Image (Self : in Cpu_Timer_Instance) return String is
   begin
      return Duration'Image (Ada.Real_Time.To_Duration (Self.Result)) & " s";
   end Image;

   function Image (Self : in Wall_Timer_Instance) return String is
   begin
      return Duration'Image (Ada.Real_Time.To_Duration (Self.Result)) & " s";
   end Image;

end Stopwatch.Representation;
