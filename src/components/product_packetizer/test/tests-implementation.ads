--------------------------------------------------------------------------------
-- Product_Packetizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Product Packetizer.
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the packetizing of packets in a nominal situation.
   overriding procedure Test_Nominal_Packetizing (Self : in out Instance);
   -- This unit tests enabling and disabling a packet via command.
   overriding procedure Test_Packet_Enable_Disable (Self : in out Instance);
   -- This unit tests changing a packet's period by command.
   overriding procedure Test_Packet_Set_Period (Self : in out Instance);
   -- This unit test tests the component's response to receiving a missing data product.
   overriding procedure Test_Missing_Data_Product (Self : in out Instance);
   -- This unit test tests the component's response to receiving a bad ID status in response to a data product request.
   overriding procedure Test_Bad_Id_Data_Product (Self : in out Instance);
   -- This unit test tests the component's response to receiving a data product with an unexpected size.
   overriding procedure Test_Data_Product_Size_Mismatch (Self : in out Instance);
   -- This unit tests the component's behavior when rolling over its internal count.
   overriding procedure Test_Roll_Over (Self : in out Instance);
   -- This unit tests the component's behavior when sending commands with bad packet ids
   overriding procedure Test_Bad_Commands (Self : in out Instance);
   -- This unit tests the component's ability to respond to a Send Packet command.
   overriding procedure Test_Send_Packet_Command (Self : in out Instance);
   -- This unit tests a packet with an offset and makes sure packets come out at the correct time.
   overriding procedure Test_Offset (Self : in out Instance);
   -- This unit tests a packet with padding.
   overriding procedure Test_Padding (Self : in out Instance);
   -- This unit tests a packet with a period of zero. It should behave just like disabled.
   overriding procedure Test_Zero_Period (Self : in out Instance);
   -- This unit test tests a command being dropped due to a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);
   -- This unit test tests the special packet period items that can be emitted inside a product packetizer packet.
   overriding procedure Test_Packet_Period_Items (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
