--------------------------------------------------------------------------------
-- Product_Packetizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Product Packetizer.
package Tests.Implementation is

   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the basic on-change functionality where packets are only
   -- sent when data products have changed since the last emission.
   overriding procedure Test_On_Change_Nominal (Self : in out Instance);
   -- This unit test tests that data products with used_for_on_change set to False do
   -- not trigger packet emission in on-change mode.
   overriding procedure Test_On_Change_Used_For_On_Change_False (Self : in out Instance);
   -- This unit test tests the Enable_Packet_On_Change command to dynamically switch
   -- a packet to on-change mode and verifies the correct event is emitted.
   overriding procedure Test_Enable_Packet_On_Change_Command (Self : in out Instance);
   -- This unit test tests multiple data product changes over time to verify emission
   -- time tracking works correctly.
   overriding procedure Test_On_Change_Multiple_Changes (Self : in out Instance);
   -- This unit test tests that on-change packets respect their evaluation period and
   -- only check for changes on period boundaries.
   overriding procedure Test_On_Change_With_Period (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
