--------------------------------------------------------------------------------
-- Product_Copier Tests Spec
--------------------------------------------------------------------------------

-- This is a set of unit tests for the Product_Copier component.
package Product_Copier_Tests.Implementation is

   -- Test data and state:
   type Instance is new Product_Copier_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- Tests whether two conflicting destinations raise an error.
   overriding procedure Test_Dest_Conflict (Self : in out Instance);
   -- Tests the fetch and send operations caused by a tick.
   overriding procedure Test_Nominal_Tick (Self : in out Instance);
   -- Tests that no data products are sent to the destination when a fetch fails.
   overriding procedure Test_Fetch_Fail_Behavior (Self : in out Instance);
   -- Makes sure an event is raised when a fetch operation fails and the
   -- corresponding init flag is set.
   overriding procedure Test_Fetch_Fail_Event (Self : in out Instance);

   -- Test data and state:
   type Instance is new Product_Copier_Tests.Base_Instance with record
      null;
   end record;
end Product_Copier_Tests.Implementation;
