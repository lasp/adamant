-- Tell the compiler that we are using Ravenscar
--pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;

with Aa;
with Bb;
with Global; use Global;

procedure Test is
   Data_A : constant Aa.T := (One => 17, Two => 23, Three => 5);
   Data_B : constant Bb.T := (Element => 31, Element2 => 34);
begin
   Put_Line ("Attaching component connectors... ");
   Tester.Init_Base (Queue_Size => 100);
   Comp.Init_Base (Queue_Size => 100);
   Comp.Attach_Generic_Type_1_Send (Tester'Unchecked_Access, Tester.Generic_Type_1_Recv_Sync_Access);
   Tester.Attach_Generic_Type_2_Send (Comp'Unchecked_Access, Comp.Generic_Type_2_Recv_Async_Access);
   Tester.Attach_Aa_T_Send (Comp'Unchecked_Access, Comp.Aa_T_Recv_Sync_Access);
   Put_Line ("passed.");
   New_Line;

   Put_Line ("Sending data on connectors... ");
   Tester.Generic_Type_2_Send (Data_B);
   Tester.Aa_T_Send (Data_A);
   pragma Assert (Tester.Generic_Type_1_Recv_Sync_History.Get_Count = 1);
   Put_Line ("passed.");
   New_Line;

   Tester.Final_Base;
   Comp.Final_Base;
   --  Sentinel for the cross test runner.
   Put_Line ("=== ALL TESTS PASSED ===");
end Test;
