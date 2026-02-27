--------------------------------------------------------------------------------
-- Command_Rejector Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Command_Rejector_Reciprocal;
with Sys_Time;
with Printable_History;
with Command.Representation;
with Data_Product.Representation;
with Event.Representation;
with Packet.Representation;
with Sys_Time.Representation;
with Data_Product;
with Packed_U16.Representation;
with Event;
with Command_Header.Representation;

-- This component is initialized with a list of commands to reject. The component receives commands, and checks their IDs against the reject command list. If a command is found in the list, then it is dropped and reported as an error packet. Commands that are not on the reject list are always forwarded. The reject command list is stored internally as a binary tree data structure that can determine if a command should be rejected or not in O(log(n)) time, where n is the number of commands to reject. Since most systems only manage a handful of commands on the reject list, the performance of this component should be acceptable for most missions. A common application for this component is to actively disallow commands emanating from certain sources, such as an onboard command sequence.
package Component.Command_Rejector.Implementation.Tester is

   use Component.Command_Rejector_Reciprocal;
   -- Invoker connector history packages:
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Rejected_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);

   -- Data product history packages:
   package Rejected_Command_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Error_Packet_History_Package is new Printable_History (Command.T, Command.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Command_Rejector_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Command_Rejector.Implementation.Instance;
      -- Connector histories:
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Rejected_Command_History : Rejected_Command_History_Package.Instance;
      -- Data product histories:
      Rejected_Command_Count_History : Rejected_Command_Count_History_Package.Instance;
      -- Packet histories:
      Error_Packet_History : Error_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The packet send connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Error packets are sent out of this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was rejected (dropped) because it was found in the reject list.
   overriding procedure Rejected_Command (Self : in out Instance; Arg : in Command_Header.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Command Rejector.
   -- The number of received commands rejected because they were on the reject list.
   overriding procedure Rejected_Command_Count (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Command Rejector component.
   -- This packet contains a command that was dropped due to being on the reject list.
   overriding procedure Error_Packet (Self : in out Instance; Arg : in Command.T);

end Component.Command_Rejector.Implementation.Tester;
