--------------------------------------------------------------------------------
-- Stack_Monitor Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Stack_Monitor_Reciprocal;
with Sys_Time;
with Printable_History;
with Packet.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Event;
with Packed_U16.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;

-- This component produces a packet holding the stack and secondary stack usage for all tasks configured for a particular assembly. It is provided an autocoded data structure upon initialization that contains the tasks which it is to monitor. The packet produced contains a byte representing the percent usage of the stack and secondary stack for each task. Task usage is recalculated on every tick.
package Component.Stack_Monitor.Implementation.Tester is

   use Component.Stack_Monitor_Reciprocal;
   -- Invoker connector history packages:
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Packet_Period_Set_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Packet_Period_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Stack_Usage_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Stack_Monitor_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Stack_Monitor.Implementation.Instance;
      -- Connector histories:
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Packet_Period_Set_History : Packet_Period_Set_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Packet_Period_History : Packet_Period_History_Package.Instance;
      -- Packet histories:
      Stack_Usage_Packet_History : Stack_Usage_Packet_History_Package.Instance;
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
   -- Send a packet of stack and secondary stack usage.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received to change the packet period.
   overriding procedure Packet_Period_Set (Self : in out Instance; Arg : in Packed_U16.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Stack Monitor component.
   -- The current packet period.
   overriding procedure Packet_Period (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the stack monitor.
   -- This packet contains stack and secondary stack usage numbers for tasks in the system.
   overriding procedure Stack_Usage_Packet (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Custom functions for whitebox testing:
   -----------------------------------------------
   not overriding procedure Check_Stack_Indexes (Self : in out Instance; Arg : in Index_Array_Type);

end Component.Stack_Monitor.Implementation.Tester;
