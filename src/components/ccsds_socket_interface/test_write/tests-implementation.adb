--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Socket_Address.Assertion; use Socket_Address.Assertion;
with Ccsds_Space_Packet.Representation;
with Ccsds_Primary_Header;
with Interfaces;
with Ccsds_Enums; use Ccsds_Enums;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      Thefile : File_Type;
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

   overriding procedure Test_Packet_Send (Self : in out Instance) is
      use Ccsds_Primary_Header;
      use Interfaces;
      use GNAT.Sockets;
      T : Component.Ccsds_Socket_Interface.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet_Good : constant Ccsds_Space_Packet.T := (Header => (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (15), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented, Sequence_Count => Ccsds_Sequence_Count_Type (22), Packet_Length => 10 - 1), Data => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, others => 0]);
      Packet_Bad : constant Ccsds_Space_Packet.T := (Header => (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (12), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented, Sequence_Count => Ccsds_Sequence_Count_Type (17), Packet_Length => 50), Data => [99, 98, 97, 96, 95, 94, others => 17]);
      Bad_Addr : constant Socket_Address.T := (Ip_Address => [127, 0, 0, 1], Port => Self.Port - 1);
      Address : constant Socket_Address.T := (Ip_Address => [127, 0, 0, 1], Port => Self.Port);
   begin
      Put_Line ("Starting test.");

      -- Expected to send packet:
      Put_Line ("Expected to send packet:");
      Put_Line (Ccsds_Space_Packet.Representation.Image (Packet_Good));

      -- Run the init method to connect the socket, expect error:
      Self.Tester.Component_Instance.Init (Addr => "127.0.0.1", Port => Natural (Self.Port) - 1);
      Natural_Assert.Eq (T.Socket_Connected_History.Get_Count, 0);
      Natural_Assert.Eq (T.Socket_Not_Connected_History.Get_Count, 1);
      Socket_Address_Assert.Eq (T.Socket_Not_Connected_History.Get (1), Bad_Addr);

      -- Send a few messages to the component, expect these to be dropped.
      for Idx in 1 .. 4 loop
         -- Send the buffer to the component:
         T.Ccsds_Space_Packet_T_Send (Packet_Bad);
         -- Execute the component:
         Natural_Assert.Eq (T.Dispatch_All, 1);
      end loop;

      -- Run the init method to connect the socket, expect success::
      Self.Tester.Component_Instance.Init (Addr => "127.0.0.1", Port => Natural (Self.Port));
      Natural_Assert.Eq (T.Socket_Connected_History.Get_Count, 1);
      Natural_Assert.Eq (T.Socket_Not_Connected_History.Get_Count, 1);
      Socket_Address_Assert.Eq (T.Socket_Connected_History.Get (1), Address);

      -- Send a few messages to the component:
      for Idx in 5 .. 8 loop
         -- Send the buffer to the component:
         T.Ccsds_Space_Packet_T_Send (Packet_Good);
         -- Execute the component:
         Natural_Assert.Eq (T.Dispatch_All, 1);
      end loop;
   end Test_Packet_Send;

end Tests.Implementation;
