--------------------------------------------------------------------------------
-- Ccsds_Router Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Router.
package Ccsds_Router_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Router_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests all permutations of initializing the component and makes sure improper initialization results in a runtime assertion.
   overriding procedure Test_Initialization (Self : in out Instance);
   -- This unit test tests that CCSDS packets are routed to the expected destination component based on the routing table.
   overriding procedure Test_Nominal_Routing (Self : in out Instance);
   -- This unit test makes sure that packets with APIDs not found in the routing table are dropped and reported appropriately.
   overriding procedure Test_Unrecognized_Id (Self : in out Instance);
   -- This unit test tests that when the component's internal queue is overflowed, that the packet is dropped and reported appropriately.
   overriding procedure Test_Dropped_Packet (Self : in out Instance);
   -- This unit test tests that CCSDS packets of non-contiguous sequence counts produce a warning event.
   overriding procedure Test_Sequence_Count_Warning (Self : in out Instance);
   -- This unit test tests that CCSDS packets of identical subsequent sequence counts get dropped.
   overriding procedure Test_Duplicate_Packet_Drop (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Router_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Router_Tests.Implementation;
