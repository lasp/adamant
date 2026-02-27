--------------------------------------------------------------------------------
-- Cpu_Monitor Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CPU Monitor component. Testing the actual correctness of the produced CPU monitor packet is not easy to do at the unit test level. This will be done at an integrated test level. The unit tests below make sure commanding and packet generation of the component works as expected.
package Cpu_Monitor_Tests.Implementation is
   -- Test data and state:
   type Instance is new Cpu_Monitor_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises the command to change the packet creation rate.
   overriding procedure Test_Packet_Period (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Cpu_Monitor_Tests.Base_Instance with record
      null;
   end record;
end Cpu_Monitor_Tests.Implementation;
