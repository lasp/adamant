--------------------------------------------------------------------------------
-- Background_Component Tests Spec
--------------------------------------------------------------------------------

-- This is a set of unit tests for the Background Component.
package Background_Component_Tests.Implementation is
   -- Test data and state:
   type Instance is new Background_Component_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test tests the nominal behavior of the component.
   overriding procedure Test_Nominal (Self : in out Instance);

   -- Test data and state:
   type Instance is new Background_Component_Tests.Base_Instance with record
      null;
   end record;
end Background_Component_Tests.Implementation;
