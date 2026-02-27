--------------------------------------------------------------------------------
-- Event_Filter_Entry Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the event_filter_entry package
package Event_Filter_Entry_Tests.Implementation is
   -- Test data and state:
   type Instance is new Event_Filter_Entry_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test is intended to test the different permutations of the init of the byte array.
   overriding procedure Test_Init_List (Self : in out Instance);
   -- This unit test tests changing the event state to a desired value for a given ID.
   overriding procedure Test_Set_Event_State (Self : in out Instance);
   -- The unit test that makes sure the count for the number filtered increments as expected, can be retrieved, and reset.
   overriding procedure Test_Event_Filtered_Count (Self : in out Instance);
   -- This unit test that tests the count for the number unfiltered events and that it increments as expected, can be retrieved, and reset.
   overriding procedure Test_Event_Unfiltered_Count (Self : in out Instance);
   -- Unit test to get the range of the event ids for the component
   overriding procedure Test_Get_Event_Range (Self : in out Instance);
   -- A unit test that will just test getting and setting a global state from enabled and disabled
   overriding procedure Test_Global_Enable_Switch (Self : in out Instance);
   -- Test to make sure that we appropriately get the byte array access for packetizing in the component.
   overriding procedure Test_Get_Entry_Array (Self : in out Instance);

   -- Test data and state:
   type Instance is new Event_Filter_Entry_Tests.Base_Instance with record
      null;
   end record;
end Event_Filter_Entry_Tests.Implementation;
