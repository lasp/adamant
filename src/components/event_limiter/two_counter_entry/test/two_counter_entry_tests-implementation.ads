--------------------------------------------------------------------------------
-- Two_Counter_Entry Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the two_counter_entry object for help with the event_limiter
package Two_Counter_Entry_Tests.Implementation is
   -- Test data and state:
   type Instance is new Two_Counter_Entry_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private

   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test is intended to test the different permutations of the init of the byte array.
   overriding procedure Test_Init_List (Self : in out Instance);
   -- This unit test tests incrementing the appropriate count when given an ID.
   overriding procedure Test_Increment_Count (Self : in out Instance);
   -- This unit test tests decrementing the appropriate count when given an ID.
   overriding procedure Test_Decrement_Count (Self : in out Instance);
   -- This unit test tests changing the enable status to a desired value for a given ID.
   overriding procedure Test_Set_Enable_State (Self : in out Instance);
   -- This unit test is to test changing the persisence count
   overriding procedure Test_Set_Persistence (Self : in out Instance);
   -- The unit test that makes sure the count increments as expected, can be retrieved, and reset.
   overriding procedure Test_Event_Limited_Count (Self : in out Instance);
   -- Unit test to get the range of the event ids for the component
   overriding procedure Test_Get_Event_Range (Self : in out Instance);
   -- A unit test that will just test getting and setting a master state from enabled and disabled
   overriding procedure Test_Master_Enable_Switch (Self : in out Instance);

   -- Test data and state:
   type Instance is new Two_Counter_Entry_Tests.Base_Instance with record
      null;
   end record;
end Two_Counter_Entry_Tests.Implementation;
