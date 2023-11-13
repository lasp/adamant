--------------------------------------------------------------------------------
-- Router_Table Tests Spec
--------------------------------------------------------------------------------

with Router_Table;

-- This is a unit test suite for the router_table object
package Router_Table_Tests.Implementation is
   -- Test data and state:
   type Instance is new Router_Table_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test adds registration elemenents to the router table and asserts for correct response and table structure
   overriding procedure Add_To_Table (Self : in out Instance);

   -- Test data and state:
   type Instance is new Router_Table_Tests.Base_Instance with record
      Table : Router_Table.Instance_Access;
   end record;
end Router_Table_Tests.Implementation;
