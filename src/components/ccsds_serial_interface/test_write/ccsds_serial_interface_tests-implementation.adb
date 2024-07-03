--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Ccsds_Primary_Header;
with Ccsds_Space_Packet;
with Interfaces;
with Ccsds_Enums; use Ccsds_Enums;

package body Ccsds_Serial_Interface_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Packet_Send (Self : in out Instance) is
      use Ccsds_Primary_Header;
      use Interfaces;
      T : Component.Ccsds_Serial_Interface.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet_Good : constant Ccsds_Space_Packet.T :=
         (Header =>
             (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (15), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
               Sequence_Count => Ccsds_Sequence_Count_Type (22), Packet_Length => 10 - 1),
          Data => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, others => 0]);
   begin
      -- Put_Line("Starting test.");

      -- Expected to send packet:
      -- Put_Line("Expected to send packet:");
      -- Put_Line(Ccsds_Space_Packet.Representation.Image(packet_good));

      -- Send a few messages to the component:
      for Idx in 5 .. 8 loop
         -- Send the buffer to the component:
         T.Ccsds_Space_Packet_T_Send (Packet_Good);
         -- Execute the component:
         Natural_Assert.Eq (T.Dispatch_All, 1);
      end loop;
   end Test_Packet_Send;

end Ccsds_Serial_Interface_Tests.Implementation;
