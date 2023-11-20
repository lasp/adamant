--------------------------------------------------------------------------------
-- Ccsds_Router Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Router, when the unrecognized apid connector is connected.
package Unrecognized_Apid_Tests.Implementation is
   -- Test data and state:
   type Instance is new Unrecognized_Apid_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure that packets with APIDs not found in the routing table are forwarded and reported appropriately.
   overriding procedure Test_Unrecognized_Id (Self : in out Instance);
   -- This unit test makes sure that packets with APIDs not found in the routing table are forwarded and not reported appropriately.
   overriding procedure Test_Unrecognized_Id_No_Report (Self : in out Instance);

   -- Test data and state:
   type Instance is new Unrecognized_Apid_Tests.Base_Instance with record
      null;
   end record;
end Unrecognized_Apid_Tests.Implementation;
