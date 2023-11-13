--------------------------------------------------------------------------------
-- Monitor Tests Body
--------------------------------------------------------------------------------

with Monitor; use Monitor;
with Smart_Assert;

package body Monitor_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Assertion packages:
   -------------------------------------------------------------------------
   package State_Assert is new Smart_Assert.Discrete (Monitor.Monitor_State, Monitor.Monitor_State'Image);
   package Status_Assert is new Smart_Assert.Discrete (Monitor.Check_Status, Monitor.Check_Status'Image);

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Monitor_Green_To_Red (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      -- Create monitor:
      Mon : Monitor.Instance;
   begin
      -- Initialize:
      Mon.Init (Green_To_Red_Persistance_Threshold => 5, Red_To_Green_Persistance_Threshold => 1, Enabled => True);
      State_Assert.Eq (Mon.Get_State, Green);

      -- Transition to red:
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);

      -- Transition back to green:
      Status_Assert.Eq (Mon.Check (True), Red_To_Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);

      -- Transition to red with some hiccups:
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (True), Red_To_Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      State_Assert.Eq (Mon.Get_State, Green);
   end Test_Monitor_Green_To_Red;

   overriding procedure Test_Monitor_Red_To_Green (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      -- Create monitor:
      Mon : Monitor.Instance;
   begin
      -- Initialize:
      Mon.Init (Green_To_Red_Persistance_Threshold => 2, Red_To_Green_Persistance_Threshold => 5, Enabled => True);
      State_Assert.Eq (Mon.Get_State, Green);

      -- Transition to red:
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);

      -- Transition to green:
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red_To_Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);

      -- Transition back to red:
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);

      -- Transition to green with some hiccups:
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red);
      Status_Assert.Eq (Mon.Check (True), Red_To_Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);

      -- Set persistance thresholds:
      Mon.Set_Persistance_Thresholds (Green_To_Red_Persistance_Threshold => 5, Red_To_Green_Persistance_Threshold => 1);
      State_Assert.Eq (Mon.Get_State, Green);

      -- Transition to red:
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      Status_Assert.Eq (Mon.Check (False), Red);

      -- Transition back to green:
      Status_Assert.Eq (Mon.Check (True), Red_To_Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);

      -- Transition to red with some hiccups:
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      Status_Assert.Eq (Mon.Check (True), Red_To_Green);
      Status_Assert.Eq (Mon.Check (True), Green);
      pragma Unreferenced (Mon);
   end Test_Monitor_Red_To_Green;

   overriding procedure Test_Monitor_Enable_Disable (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      -- Create monitor:
      Mon : Monitor.Instance;
   begin
      -- Initialize:
      Mon.Init (Green_To_Red_Persistance_Threshold => 5, Red_To_Green_Persistance_Threshold => 1, Enabled => False);
      State_Assert.Eq (Mon.Get_State, Disabled);

      -- Check should always return disabled:
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (True), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (True), Disabled);
      Status_Assert.Eq (Mon.Check (True), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);

      -- OK now enable the monitor:
      Mon.Enable;

      -- Transition to red:
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);

      -- OK now disable monitor:
      Mon.Disable;

      -- Check should always return disabled:
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (True), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (True), Disabled);
      Status_Assert.Eq (Mon.Check (True), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);
      Status_Assert.Eq (Mon.Check (False), Disabled);

      -- OK now enable the monitor:
      Mon.Enable;

      -- Transition to red:
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green);
      State_Assert.Eq (Mon.Get_State, Green);
      Status_Assert.Eq (Mon.Check (False), Green_To_Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
      Status_Assert.Eq (Mon.Check (False), Red);
      State_Assert.Eq (Mon.Get_State, Red);
   end Test_Monitor_Enable_Disable;

end Monitor_Tests.Implementation;
