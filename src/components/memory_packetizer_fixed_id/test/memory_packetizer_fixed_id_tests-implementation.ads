--------------------------------------------------------------------------------
-- Memory_Packetizer_Fixed_Id Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Memory Packetizer Fixed Id component.
package Memory_Packetizer_Fixed_Id_Tests.Implementation is
   -- Test data and state:
   type Instance is new Memory_Packetizer_Fixed_Id_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the packetizer's normal behavior and makes sure it packetizes memory properly, metering out the packets according to its rate.
   overriding procedure Test_Nominal_Packetization (Self : in out Instance);
   -- This unit test tests the Set_Max_Packet_Rate command, and ensures that the rate changes appropriately.
   overriding procedure Test_Set_Max_Packet_Rate (Self : in out Instance);
   -- This unit test tests a bad Set_Max_Packet_Rate command, and ensures that an appropriate event is thrown.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test tests the behavior when the component's queue becomes full and must drop a memory dump request.
   overriding procedure Test_Memory_Dump_Dropped (Self : in out Instance);

   -- Test data and state:
   type Instance is new Memory_Packetizer_Fixed_Id_Tests.Base_Instance with record
      null;
   end record;
end Memory_Packetizer_Fixed_Id_Tests.Implementation;
