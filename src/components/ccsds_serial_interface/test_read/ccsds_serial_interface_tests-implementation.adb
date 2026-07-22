--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Synchronous_Task_Control;
with Basic_Assertions; use Basic_Assertions;
with Ccsds_Space_Packet.Representation;
with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Ccsds_Primary_Header;
with Interfaces;
with Task_Types;
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

   overriding procedure Test_Packet_Receive (Self : in out Instance) is
      use Ccsds_Primary_Header;
      use Interfaces;
      T : Component.Ccsds_Serial_Interface.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Start the serial internal listener thread:
      Serial_Listener_Task_Signal : aliased Ada.Synchronous_Task_Control.Suspension_Object;
      Serial_Listener_Task_Info : aliased Task_Types.Task_Info;
      Serial_Listener_Task : Component.Ccsds_Serial_Interface.Listener_Task (Serial_Listener_Task_Info'Unchecked_Access, T.Component_Instance'Access, Serial_Listener_Task_Signal'Access, 10, 60_000, 3_000);
      Packet : Ccsds_Space_Packet.T;
      Recv_Failed_Header : Ccsds_Primary_Header.T;
      Expected : constant Ccsds_Space_Packet.T :=
         (Header =>
             (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (15), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
               Sequence_Count => Ccsds_Sequence_Count_Type (22), Packet_Length => 8 - 1),
          Data => [1, 2, 3, 4, 5, 6, 7, 8, others => 0]);
      Expected_Tlm : constant Ccsds_Space_Packet.T :=
         (Header =>
             (Version => 0, Packet_Type => Ccsds_Packet_Type.Telemetry, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (15), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
               Sequence_Count => Ccsds_Sequence_Count_Type (22), Packet_Length => 8 - 1),
          -- Note: bytes in tosend.txt must avoid 16#0A#, 16#0C# and 16#0D#. This is a
          -- test harness artifact only: the Linux test build's Diagnostic_Uart reads
          -- stdin through Ada.Text_IO, which consumes line/page terminators before the
          -- component ever sees them. The component itself never alters packet bytes.
          Data => [16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#, 16#18#, others => 0]);
   begin
      -- Expected to get packet:
      Put_Line ("Expected to get packet:");
      Put_Line (Ccsds_Space_Packet.Representation.Image (Expected));
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Start the component thread:
      Ada.Synchronous_Task_Control.Set_True (Serial_Listener_Task_Signal);

      -- Let the task do some work:
      delay Duration (0.5);

      -- Stop the component thread:
      Ada.Synchronous_Task_Control.Set_True (Serial_Listener_Task_Signal);

      -- Make sure two packets were sent out:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Packet := T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1);
      Put_Line ("Tester got packet:");
      Put_Line (Ccsds_Space_Packet.Representation.Image (Packet));

      -- Make sure that the data size matches what we expect:
      Ccsds_Space_Packet_Assert.Eq (Packet, Expected);

      -- The input stream also contains a packet whose Packet_Type field is
      -- flipped to 0 (telemetry) instead of 1 (telecommand). The listener does
      -- not filter on packet type, so the packet must be forwarded with its
      -- type field deserialized as Telemetry.
      Packet := T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2);
      Put_Line ("Tester got packet:");
      Put_Line (Ccsds_Space_Packet.Representation.Image (Packet));
      Ccsds_Space_Packet_Assert.Eq (Packet, Expected_Tlm);

      -- The input stream additionally contains a sync pattern followed by a
      -- CCSDS primary header whose Packet_Length field is 16#FFFF#. The
      -- resulting packet cannot fit in the packet buffer, so the listener must
      -- reject it with a Packet_Recv_Failed event and must not forward it.
      -- These assertions prove the guard rejects the maximum possible length
      -- value.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Recv_Failed_History.Get_Count, 1);
      Recv_Failed_Header := T.Packet_Recv_Failed_History.Get (1);
      Natural_Assert.Eq (Natural (Recv_Failed_Header.Packet_Length), 16#FFFF#);
   end Test_Packet_Receive;

end Ccsds_Serial_Interface_Tests.Implementation;
