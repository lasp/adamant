--------------------------------------------------------------------------------
-- Memory_Packetizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Memory Packetizer component.
package Memory_Packetizer_Tests.Implementation is
   -- Test data and state:
   type Instance is new Memory_Packetizer_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the packetizer's normal behavior and make sure it packetizes memory properly, metering out the packets according to its rate.
   overriding procedure Test_Nominal_Packetization (Self : in out Instance);
   -- This unit test tests the Set_Max_Packet_Rate commmand, and ensures that the rate changes appropriately.
   overriding procedure Test_Set_Max_Packet_Rate (Self : in out Instance);
   -- This unit test tests a bad Set_Max_Packet_Rate commmand, and ensures that an appropriate event is thrown.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test tests the behavior when the component's queue becomes full and must drop a memory dump request.
   overriding procedure Test_Memory_Dump_Dropped (Self : in out Instance);
   -- This unit test tests what happens when more packet ids are sent to the component than the component can track sequence counts for.
   overriding procedure Test_Max_Packet_Id_Exceeded (Self : in out Instance);

   -- Test data and state:
   type Instance is new Memory_Packetizer_Tests.Base_Instance with record
      null;
   end record;
end Memory_Packetizer_Tests.Implementation;
