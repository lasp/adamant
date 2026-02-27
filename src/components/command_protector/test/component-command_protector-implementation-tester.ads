--------------------------------------------------------------------------------
-- Command_Protector Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Command_Protector_Reciprocal;
with Sys_Time;
with Printable_History;
with Command.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Packet.Representation;
with Sys_Time.Representation;
with Event;
with Command_Header.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;
with Packed_Arm_State.Representation;
with Packed_Arm_Timeout.Representation;
with Packed_U16.Representation;

-- This component is initialized with a list of protected commands. The component receives commands, and checks their IDs against the protected commands list. If a command is found in the list, then it is only forwarded if the component has been 'armed', otherwise the command is dropped and an error packet is produced with the rejected command data. Commands that are not on the protected commands list are always forwarded. To 'arm' the component, a special 'arm' command must be sent to the component to transition it to the 'armed' state. At this point, a protected command may be successfully forwarded. Note that after the receipt of any command, the component will transition back to the 'unarmed' state, rejecting any subsequently received protected commands until another 'arm' command is received. The component will also transition back to the 'unarmed' state after a timeout expires, which is set in the 'arm' command itself. The protected command list is stored internally as a binary tree data structure that can determine if a command is protected or not in O(log(n)) time, where n is the number of protected commands. Since most systems only manage a handful of protected commands, the performance of this component should be acceptable for most missions.
package Component.Command_Protector.Implementation.Tester is

   use Component.Command_Protector_Reciprocal;
   -- Invoker connector history packages:
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Rejected_Protected_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Accepted_Protected_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Armed_History_Package is new Printable_History (Packed_Arm_Timeout.T, Packed_Arm_Timeout.Representation.Image);
   package Unarmed_History_Package is new Printable_History (Natural, Natural'Image);
   package Unarmed_Timeout_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Armed_State_History_Package is new Printable_History (Packed_Arm_State.T, Packed_Arm_State.Representation.Image);
   package Armed_State_Timeout_History_Package is new Printable_History (Packed_Arm_Timeout.T, Packed_Arm_Timeout.Representation.Image);
   package Protected_Command_Reject_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Protected_Command_Forward_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Error_Packet_History_Package is new Printable_History (Command.T, Command.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Command_Protector_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Command_Protector.Implementation.Instance;
      -- Connector histories:
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Rejected_Protected_Command_History : Rejected_Protected_Command_History_Package.Instance;
      Accepted_Protected_Command_History : Accepted_Protected_Command_History_Package.Instance;
      Armed_History : Armed_History_Package.Instance;
      Unarmed_History : Unarmed_History_Package.Instance;
      Unarmed_Timeout_History : Unarmed_Timeout_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Armed_State_History : Armed_State_History_Package.Instance;
      Armed_State_Timeout_History : Armed_State_Timeout_History_Package.Instance;
      Protected_Command_Reject_Count_History : Protected_Command_Reject_Count_History_Package.Instance;
      Protected_Command_Forward_Count_History : Protected_Command_Forward_Count_History_Package.Instance;
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
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
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
   -- A protected command was rejected (dropped) because the component was unarmed.
   overriding procedure Rejected_Protected_Command (Self : in out Instance; Arg : in Command_Header.T);
   -- A protected command was accepted because the component was armed.
   overriding procedure Accepted_Protected_Command (Self : in out Instance; Arg : in Command_Header.T);
   -- The component received the arm command and is now armed.
   overriding procedure Armed (Self : in out Instance; Arg : in Packed_Arm_Timeout.T);
   -- The component received a command and is now unarmed.
   overriding procedure Unarmed (Self : in out Instance);
   -- The component armed state timed out and is now unarmed.
   overriding procedure Unarmed_Timeout (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Command Protector.
   -- The current armed/unarmed state of the component.
   overriding procedure Armed_State (Self : in out Instance; Arg : in Packed_Arm_State.T);
   -- The time remaining (in ticks) until the armed state expires.
   overriding procedure Armed_State_Timeout (Self : in out Instance; Arg : in Packed_Arm_Timeout.T);
   -- The number of protected commands rejected because the component was unarmed.
   overriding procedure Protected_Command_Reject_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of protected commands forwarded because the component was armed.
   overriding procedure Protected_Command_Forward_Count (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Command Protector component.
   -- This packet contains a command that was dropped due to error.
   overriding procedure Error_Packet (Self : in out Instance; Arg : in Command.T);

end Component.Command_Protector.Implementation.Tester;
