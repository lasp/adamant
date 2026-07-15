--------------------------------------------------------------------------------
-- Product_Store Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Product Store component.
package Product_Store_Tests.Implementation is

   -- Test data and state:
   type Instance is new Product_Store_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests saving the data products to the store upon receipt of a
   -- tick.
   overriding procedure Test_Nominal_Save (Self : in out Instance);
   -- This unit test tests restoring the data products from the store by command,
   -- including all of the restore timestamp modes.
   overriding procedure Test_Nominal_Restore (Self : in out Instance);
   -- This unit test tests restoring the data products from the store at Set_Up, both
   -- with a valid and an invalid store.
   overriding procedure Test_Restore_On_Set_Up (Self : in out Instance);
   -- This unit test tests the component's response to a restore command when the
   -- store contents are corrupted.
   overriding procedure Test_Crc_Invalid_On_Restore (Self : in out Instance);
   -- This unit test tests saving the data products to the store by command.
   overriding procedure Test_Save_Command (Self : in out Instance);
   -- This unit test tests the component's response to a data product that is missing
   -- from the database on save, both when the existing store contents are valid and
   -- when they are not.
   overriding procedure Test_Missing_Data_Product (Self : in out Instance);
   -- This unit test tests the component's response to a fetched data product with an
   -- unexpected length.
   overriding procedure Test_Length_Mismatch (Self : in out Instance);
   -- This unit test tests dumping the contents of the store into a packet by
   -- command.
   overriding procedure Test_Dump_Store (Self : in out Instance);
   -- This unit test tests the component's response to an invalid command.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test tests a command being dropped due to a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);

   -- Test data and state:
   type Instance is new Product_Store_Tests.Base_Instance with record
      null;
   end record;
end Product_Store_Tests.Implementation;
