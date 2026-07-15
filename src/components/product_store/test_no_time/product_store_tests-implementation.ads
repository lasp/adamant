--------------------------------------------------------------------------------
-- Product_Store Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Product Store component with a store
-- configured without a save time.
package Product_Store_Tests.Implementation is

   -- Test data and state:
   type Instance is new Product_Store_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests saving the data products to a store configured without a
   -- save time upon receipt of a tick.
   overriding procedure Test_Save_No_Time (Self : in out Instance);
   -- This unit test tests restoring the data products from a store configured
   -- without a save time.
   overriding procedure Test_Restore_No_Time (Self : in out Instance);
   -- This unit test tests saving the data products to a store configured without a
   -- save time by command.
   overriding procedure Test_Save_Command_No_Time (Self : in out Instance);

   -- Test data and state:
   type Instance is new Product_Store_Tests.Base_Instance with record
      null;
   end record;
end Product_Store_Tests.Implementation;
