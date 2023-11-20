-- Tell the compiler that we are using Ravenscar
with Ada.Text_IO; use Ada.Text_IO;
with Bb, Aa;
use Aa;

package body Test_Sequential is
   procedure Test is
      Data_A : constant Aa.T := (One => 4, Two => 20, Three => 20);
      Data_B : constant Bb.T := (Element => 4, Element2 => 20);
   begin
      Put_Line ("Initializing heap... ");
      Tester.Init_Base (Queue_Size => 100);
      Put_Line ("passed.");
      New_Line;

      Put_Line ("Attaching component connectors... ");
      Tester.Connect;
      Put_Line ("passed.");
      New_Line;

      Put_Line ("Sending data to component... ");
      Tester.Bb_T_Send (Data_B);
      Put_Line ("passed.");
      New_Line;

      Put_Line ("Scheduling component execution... ");
      Tester.Sched_Send (1);
      pragma Assert (Tester.Aa_T_Recv_Sync_History.Get_Count = 1);
      pragma Assert (Data_A = Tester.Aa_T_Recv_Sync_History.Get (1));
      Put_Line ("passed.");
   end Test;
end Test_Sequential;
