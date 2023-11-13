-- Tell the compiler that we are using Ravenscar
with Ada.Text_IO; use Ada.Text_IO;
with Bb, Aa;
use Aa;
with Ada.Real_Time;

package body Test_Concurrent is
   procedure Test is
      Data_A : constant Aa.T := (One => 4, Two => 20, Three => 20);
      Data_B : constant Bb.T := (Element => 4, Element2 => 20);
      Three_Seconds : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (3_000);
      Three_Seconds_Later : Ada.Real_Time.Time;
      use Ada.Real_Time;
   begin
      Put_Line ("Initializing heap... ");
      Tester.Init_Base (Queue_Size => 100);
      Put_Line ("passed.");
      New_Line;

      Put_Line ("Attaching component connectors... ");
      Tester.Connect;
      Put_Line ("passed.");
      New_Line;

      Three_Seconds_Later := Ada.Real_Time.Clock + Three_Seconds;
      delay until Three_Seconds_Later;

      Put_Line ("Starting task... ");
      Ada.Synchronous_Task_Control.Set_True (Component_Signal);
      Put_Line ("passed.");
      New_Line;

      Put_Line ("Sending data to component... ");
      Tester.Bb_T_Send (Data_B);
      Put_Line ("passed.");
      New_Line;

      Three_Seconds_Later := Ada.Real_Time.Clock + Three_Seconds;
      delay until Three_Seconds_Later;

      Put_Line (Natural'Image (Tester.Aa_T_Recv_Sync_History.Get_Count));
      pragma Assert (Tester.Aa_T_Recv_Sync_History.Get_Count = 1);
      pragma Assert (Data_A = Tester.Aa_T_Recv_Sync_History.Get (1));

      Put_Line ("Asking task to terminate... ");
      Ada.Synchronous_Task_Control.Set_True (Component_Signal);
      Tester.Component_Instance.Stop_Task;
      Put_Line ("passed.");
   end Test;
end Test_Concurrent;
