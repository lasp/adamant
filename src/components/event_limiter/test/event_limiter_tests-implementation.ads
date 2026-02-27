--------------------------------------------------------------------------------
-- Event_Limiter Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Event Limiter component
package Event_Limiter_Tests.Implementation is
   -- Test data and state:
   type Instance is new Event_Limiter_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Mimic bit num type for helper function
   type Bit_Num is mod 8;
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test is to test incrementing counters for the range events as they come in with both enabled and disabled states.
   overriding procedure Test_Received_Event (Self : in out Instance);
   -- This unit test is to test the decrement of all the event counters in the tick and the construction of the event if events were limited.
   overriding procedure Test_Decrement_Event_Count (Self : in out Instance);
   -- This unit test sends the issue command and test that the appropriate values are received in that packet.
   overriding procedure Test_Issue_State_Packet (Self : in out Instance);
   -- This unit test is used to test enabling and disabling a single event and testing if the packet is sent if issued by the command.
   overriding procedure Test_Command_Single_State_Change (Self : in out Instance);
   -- This unit test is used to test enabling and disabling a range of events and testing if the packet is sent if issued by the command.
   overriding procedure Test_Command_Range_State_Change (Self : in out Instance);
   -- This unit test is used to test enabling and disabling all events and testing if the packet is sent if issued by the command.
   overriding procedure Test_Command_Component_State_Change (Self : in out Instance);
   -- This unit test exercises updating the persistence and ensures it was set correctly.
   overriding procedure Test_Persistence_Change (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Event_Limiter_Tests.Base_Instance with record
      null;
   end record;
end Event_Limiter_Tests.Implementation;
