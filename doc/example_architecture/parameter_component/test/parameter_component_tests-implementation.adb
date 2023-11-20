--------------------------------------------------------------------------------
-- Parameter_Component Tests Body
--------------------------------------------------------------------------------

with Parameter_Enums; use Parameter_Enums.Parameter_Update_Status;
with Parameter_Enums.Assertion; use Parameter_Enums.Assertion;
with Parameter.Assertion; use Parameter.Assertion;

package body Parameter_Component_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Unit_Test (Self : in out Instance) is
      Status : Parameter_Enums.Parameter_Update_Status.E;
      Param : Parameter.T;
   begin
      -- Send some ticks:
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Stage the unit test parameter:
      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.Start_Count ((Value => 5)));
      Parameter_Update_Status_Assert.Eq (Status, Success);

      -- Fetch the parameter to make sure the parameter value
      -- inside the component is what we just set it to.
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_Start_Count_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Parameter_Assert.Eq (Param, Self.Tester.Parameters.Start_Count ((Value => 5)));

      -- Update the next unit test parameter:
      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.Hello_World_Value ((Value => 7)));
      Parameter_Update_Status_Assert.Eq (Status, Success);

      -- Fetch the parameter to make sure the parameter value
      -- inside the component is what we just set it to.
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_Hello_World_Value_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Parameter_Assert.Eq (Param, Self.Tester.Parameters.Hello_World_Value ((Value => 7)));

      -- Now tell the component that we are done staging parameters
      -- so that they can be updated by the component.
      -- This will send a parameter to the component with an "Update"
      -- operation. This tells the base package that all parameters
      -- have been sent and it is safe to do an update.
      Parameter_Update_Status_Assert.Eq (Self.Tester.Update_Parameters, Success);

      -- After 3 more ticks, hello world should be printed.
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
   end Unit_Test;

end Parameter_Component_Tests.Implementation;
