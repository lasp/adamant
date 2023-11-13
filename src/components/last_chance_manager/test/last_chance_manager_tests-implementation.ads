--------------------------------------------------------------------------------
-- Last_Chance_Manager Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Last Chance Manager component.
package Last_Chance_Manager_Tests.Implementation is
   -- Test data and state:
   type Instance is new Last_Chance_Manager_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure the region dump command executes successfully.
   overriding procedure Test_Region_Dump (Self : in out Instance);
   -- This unit test makes sure the region clear command executes successfully.
   overriding procedure Test_Region_Clear (Self : in out Instance);
   -- This unit test makes sure an invalid command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Last_Chance_Manager_Tests.Base_Instance with record
      null;
   end record;
end Last_Chance_Manager_Tests.Implementation;
