--------------------------------------------------------------------------------
-- Event_Packetizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Event Packetizer component
package Event_Packetizer_Tests.Implementation is
   -- Test data and state:
   type Instance is new Event_Packetizer_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises the nominal behavior of the event packetizer with 3 internal packets.
   overriding procedure Test_Nominal_Packetization (Self : in out Instance);
   -- This unit test exercises the partial packet timeout feature.
   overriding procedure Test_Partial_Packet_Timeout (Self : in out Instance);
   -- This unit test exercises the partial packet timeout feature with value set to 1, which means timeout should always occur.
   overriding procedure Test_Partial_Packet_Timeout_Of_1 (Self : in out Instance);
   -- This unit test tells the packetizer to packetize a partial packet via command.
   overriding procedure Test_Commanded_Packetization (Self : in out Instance);
   -- This unit test exercises the behavior of the packetizer when it is so full that events begin getting dropped.
   overriding procedure Test_Dropped_Events (Self : in out Instance);
   -- This unit test exercises the behavior of the packetizer when it is uninitialized.
   overriding procedure Uninitialized (Self : in out Instance);

   -- Test data and state:
   type Instance is new Event_Packetizer_Tests.Base_Instance with record
      null;
   end record;
end Event_Packetizer_Tests.Implementation;
