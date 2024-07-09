--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Synchronous_Task_Control;
with Basic_Assertions; use Basic_Assertions;
with Socket_Address.Assertion; use Socket_Address.Assertion;
with Ccsds_Space_Packet.Representation;
with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Ccsds_Primary_Header;
with Interfaces;
with Task_Types;
with Ccsds_Enums; use Ccsds_Enums;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      Thefile : File_Type;
      Ignore : File_Type;
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Collect the hostname and port from a file so we can use them
      -- to initialize the component socket.
      Open (File => Thefile, Mode => In_File, Name => "socket_config.txt");
      Self.Port := GNAT.Sockets.Port_Type'Value (Get_Line (Thefile));
      pragma Warnings (Off, """theFile"" modified by call, but value might not be referenced");
      Close (Thefile);
      pragma Warnings (On, """theFile"" modified by call, but value might not be referenced");
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
      T : Component.Ccsds_Socket_Interface.Implementation.Tester.Instance_Access renames Self.Tester;
      Address : constant Socket_Address.T := (Ip_Address => [127, 0, 0, 1], Port => Self.Port);
      -- Start the socket internal listener thread:
      Socket_Listener_Task_Signal : aliased Ada.Synchronous_Task_Control.Suspension_Object;
      Socket_Listener_Task_Info : aliased Task_Types.Task_Info;
      Socket_Listener_Task : Component.Ccsds_Socket_Interface.Listener_Task (Socket_Listener_Task_Info'Unchecked_Access, T.Component_Instance'Access, Socket_Listener_Task_Signal'Access, 10, 60_000, 3_000);
      Packet : Ccsds_Space_Packet.T;
      Expected : constant Ccsds_Space_Packet.T :=
         (Header =>
             (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (15), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
               Sequence_Count => Ccsds_Sequence_Count_Type (22), Packet_Length => 8 - 1),
          Data => [1, 2, 3, 4, 5, 6, 7, 8, others => 0]);
   begin
      -- Expected to get packet:
      Put_Line ("Expected to get packet:");
      Put_Line (Ccsds_Space_Packet.Representation.Image (Expected));

      -- Run the init method to connect the socket, expect success::
      Self.Tester.Component_Instance.Init (Addr => "127.0.0.1", Port => Natural (Self.Port));
      Natural_Assert.Eq (T.Socket_Connected_History.Get_Count, 1);
      Natural_Assert.Eq (T.Socket_Not_Connected_History.Get_Count, 0);
      Socket_Address_Assert.Eq (T.Socket_Connected_History.Get (1), Address);

      -- Start the component thread:
      Ada.Synchronous_Task_Control.Set_True (Socket_Listener_Task_Signal);

      -- Let the task do some work:
      delay Duration (0.5);

      -- Stop the component thread:
      Ada.Synchronous_Task_Control.Set_True (Socket_Listener_Task_Signal);

      -- Make sure one packet was sent out:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Packet := T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1);
      Put_Line ("Tester got packet:");
      Put_Line (Ccsds_Space_Packet.Representation.Image (Packet));

      -- Make sure that the data size matches what we expect:
      Ccsds_Space_Packet_Assert.Eq (Packet, Expected);
   end Test_Packet_Receive;

end Tests.Implementation;
