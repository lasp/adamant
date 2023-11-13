-- Tell the compiler that we are using Ravenscar
--pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;

with Component.Generic_Queued.Implementation.Tester;
with Aa;
with Bb;

procedure Test is
   Data_A : constant Aa.T := (One => 17, Two => 23, Three => 5);
   Data_B : constant Bb.T := (Element => 31, Element2 => 34);
   package Generic_Component_Base is new Component.Generic_Queued (Aa.T, Bb.T, Bb.Serialized_Length);
   package Component_Package is new Generic_Component_Base.Implementation;
   package Tester_Package is new Component_Package.Tester;
   Tester : aliased Tester_Package.Instance;
   Comp : aliased Component_Package.Instance;
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
end Test;
