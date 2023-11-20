--------------------------------------------------------------------------------
-- Memory_Packetizer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Memory_Packetizer_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Data_Product;
with Event;
with Packet_Id.Representation;
with Packets_Per_Period.Representation;
with Invalid_Command_Info.Representation;
with History;

-- This active component receives memory pointer information on an asynchronous queue. It then reads the data that these pointers reference into packets, producing multiple maximum sized packets, if necessary, to packetize the entire memory region.
package Component.Memory_Packetizer.Implementation.Tester is

   use Component.Memory_Packetizer_Reciprocal;
   -- Invoker connector history packages:
   package Packet_T_Recv_Sync_History_Package is new History (Packet.T);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Max_Packet_Id_Exceeded_History_Package is new Printable_History (Packet_Id.T, Packet_Id.Representation.Image);
   package Memory_Dump_Request_Dropped_History_Package is new Printable_History (Packet_Id.T, Packet_Id.Representation.Image);
   package Max_Packet_Rate_Set_History_Package is new Printable_History (Packets_Per_Period.T, Packets_Per_Period.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Max_Packets_Per_Time_Period_History_Package is new Printable_History (Packets_Per_Period.T, Packets_Per_Period.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Memory_Packetizer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Memory_Packetizer.Implementation.Instance;
      -- Connector histories:
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Max_Packet_Id_Exceeded_History : Max_Packet_Id_Exceeded_History_Package.Instance;
      Memory_Dump_Request_Dropped_History : Memory_Dump_Request_Dropped_History_Package.Instance;
      Max_Packet_Rate_Set_History : Max_Packet_Rate_Set_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Max_Packets_Per_Time_Period_History : Max_Packets_Per_Time_Period_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Memory_Dump_Send_Dropped : Boolean := False;
      Memory_Dump_Send_Dropped_Count : Natural := 0;
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Send a packet of data.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Memory_Dump_Send message is dropped due to a full queue.
   overriding procedure Memory_Dump_Send_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump);

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The maximum number of packet ids that the component can keep track of sequence numbers for has been exceeded. Packets of this id will be emitted with a sequence number of 0.
   overriding procedure Max_Packet_Id_Exceeded (Self : in out Instance; Arg : in Packet_Id.T);
   -- The queue for memory dump requests overflowed and a request to dump memory with the given packet id was dropped.
   overriding procedure Memory_Dump_Request_Dropped (Self : in out Instance; Arg : in Packet_Id.T);
   -- A new maximum rate has been set for the packetizer.
   overriding procedure Max_Packet_Rate_Set (Self : in out Instance; Arg : in Packets_Per_Period.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Memory Packetizer component.
   -- The current maximum packet sends per time period.
   overriding procedure Max_Packets_Per_Time_Period (Self : in out Instance; Arg : in Packets_Per_Period.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Memory_Packetizer.Implementation.Tester;
