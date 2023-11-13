--------------------------------------------------------------------------------
-- Ccsds_Product_Extractor Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Ccsds Product Extractor component
package Ccsds_Product_Extractor_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Product_Extractor_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test is used to test the logic of receiving a packet that contains a data product that needs to be extracted
   overriding procedure Test_Received_Data_Product_Packet (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Product_Extractor_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Product_Extractor_Tests.Implementation;
