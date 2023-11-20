--------------------------------------------------------------------------------
-- Gps_Time Component Implementation Body
--------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Sys_Time.Arithmetic; use Sys_Time.Arithmetic;

-- See a-reatim.ads and adb
-- with System.Task_Primitives.Operations;
-- Time_Span_Unit   : constant Time_Span;
-- Time_Span_Unit   : constant Time_Span := 10#1.0#E-9;

-- Tick : constant Time_Span;
-- Tick : constant Time_Span :=
--                   Time_Span (System.Task_Primitives.Operations.RT_Resolution);

package body Component.Gps_Time.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The system time is provided via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      Ignore : Instance renames Self;
      Current_Time : constant Time := Clock;
      To_Return : Sys_Time.T;
      Status : Sys_Time_Status;
      pragma Unreferenced (Status);
   begin
      Status := To_Sys_Time (Current_Time, To_Return);
      -- To do generate an event here, maybe
      return To_Return;
   end Sys_Time_T_Return;

end Component.Gps_Time.Implementation;
