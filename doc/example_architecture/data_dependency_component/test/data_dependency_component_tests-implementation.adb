--------------------------------------------------------------------------------
-- Data_Dependency_Component Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Data_Product_Enums;

package body Data_Dependency_Component_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- System time for test. Make this non-zero to data dependencies don't report as stale.
      Self.Tester.System_Time := (10_000, 0);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Unit_Test (Self : in out Instance) is
      use Data_Product_Enums.Fetch_Status;
   begin
      -- Set data dependency values in tester component.
      Self.Tester.Counter := (Value => 10);
      Self.Tester.Temperature := (Value => 15.0);

      -- Send tick and expect nominal response.
      Put_Line ("Nominal test...");
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Put_Line ("");

      -- Send tick and expect over temp response.
      Put_Line ("Over temperature test...");
      Self.Tester.Temperature := (Value => 101.0);
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Put_Line ("");

      -- Set the data dependency time to something too old:
      Put_Line ("Stale time test...");
      -- Set timestamp of data dependency.
      Self.Tester.Data_Dependency_Timestamp_Override := (90, 0);
      -- Set simulated system time.
      Self.Tester.System_Time := (101, 0);
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Put_Line ("");

      -- Set the data dependency ID to something nonsensical:
      Put_Line ("Bad ID test...");
      Self.Tester.Data_Dependency_Return_Id_Override := 99;
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Put_Line ("");

      -- Set the return status to Not_Available:
      Self.Tester.Data_Dependency_Return_Status_Override := Not_Available;
      Put_Line ("Not available test...");
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Put_Line ("");
   end Unit_Test;

end Data_Dependency_Component_Tests.Implementation;
