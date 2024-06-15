--------------------------------------------------------------------------------
-- Product_Copier Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;

package body Product_Copier_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- TODO Call component init here.
      -- Self.Tester.Component_Instance.Init (Products_To_Copy => TBD, Send_Event_On_Source_Id_Out_Of_Range => TBD, Send_Event_On_Source_Not_Available => TBD);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- TODO Insert custom set up code here.
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- TODO Insert custom cleanup code here.
      null;
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   -- Tests whether two conflicting destinations raise an error.
   overriding procedure Test_Dest_Conflict (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test 'Test_Dest_Conflict' is unimplemented.");
   end Test_Dest_Conflict;

   -- Tests the fetch and send operations caused by a tick.
   overriding procedure Test_Nominal_Tick (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test 'Test_Nominal_Tick' is unimplemented.");
   end Test_Nominal_Tick;

   -- Tests that no data products are sent to the destination when a fetch fails.
   overriding procedure Test_Fetch_Fail_Behavior (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test 'Test_Fetch_Fail_Behavior' is unimplemented.");
   end Test_Fetch_Fail_Behavior;

   -- Makes sure an event is raised when a fetch operation fails and the
   -- corresponding init flag is set.
   overriding procedure Test_Fetch_Fail_Event (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test 'Test_Fetch_Fail_Event' is unimplemented.");
   end Test_Fetch_Fail_Event;

end Product_Copier_Tests.Implementation;
