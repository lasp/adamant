--------------------------------------------------------------------------------
-- History Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the history object
package History_Tests.Implementation is
   -- Test data and state:
   type Instance is new History_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests adding, clearing, and querying the history.
   overriding procedure Test_History (Self : in out Instance);

   -- Test data and state:
   type Instance is new History_Tests.Base_Instance with record
      null;
   end record;
end History_Tests.Implementation;
