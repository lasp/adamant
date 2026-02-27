--------------------------------------------------------------------------------
-- Product_Packetizer Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Tick;
with Command;
with Product_Packet_Types;

-- The product packetizer requests data products from an external component and packetizes them into packets at a configurable rate. The packets that this component produces are configured via an autocoded table.
--
package Component.Product_Packetizer.Implementation is

   -- The component class instance record:
   -- This component requires a list of packet descriptions which outline the packets that the component is responsible for building.
   --
   -- Discriminant Parameters:
   -- Packet_List : Product_Packet_Types.Packet_Description_List_Access_Type - The list of packets to packetize.
   --
   type Instance (Packet_List : not null Product_Packet_Types.Packet_Description_List_Access_Type) is new Product_Packetizer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to initialize the roll-over value for the packetizer's internal counter. It is calculated as the largest 32-bit multiple of all the provided periods in the Packet_List. This ensures that no packets are skipped or sent too often when a rollover occurs. Note, that this only guarantees expected roll-over behavior if the period of the packets are not changed during runtime via command. If this happens, then the user accepts that a rollover may cause unwanted behavior.
   overriding procedure Init (Self : in out Instance; Commands_Dispatched_Per_Tick : in Positive := 3);

private

   -- Special type for roll over type.
   subtype Roll_Over_Natural is Natural range Natural'First .. Natural'Last - 1;

   -- The component class instance record:
   -- This component requires a list of packet descriptions which outline the packets that the component is responsible for building.
   --
   -- Discriminant Parameters:
   -- Packet_List : Product_Packet_Types.Packet_Description_List_Access_Type - The list of packets to packetize.
   --
   type Instance (Packet_List : not null Product_Packet_Types.Packet_Description_List_Access_Type) is new Product_Packetizer.Base_Instance with record
      Count : Positive := 1; -- The count is rolled over manually in adb
      Roll_Over_Value : Roll_Over_Natural := Roll_Over_Natural'Last; -- This will change to a more appropriate value via the init function.
      Commands_Dispatched_Per_Tick : Positive := 3;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. It should be received at least as fast as the maximum desired product creation frequency.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- This is the command receive connector, used for configuring the packetizer during runtime.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the product packetizer component.
   -- Command to change the period of packet generation for a given packet id.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packet_Period.T) return Command_Execution_Status.E;
   -- Command to enable the emission of a packet from the packetizer.
   overriding function Enable_Packet (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E;
   -- Command to disable the emission of a packet from the packetizer.
   overriding function Disable_Packet (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E;
   -- Command to build specific packet and send it out on the next available tick. The packet is built and sent regardless of the packet being enabled or disabled.
   overriding function Send_Packet (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E;
   -- Command to enable the emission of a packet from the packetizer only when data products have changed since the last emission.
   overriding function Enable_Packet_On_Change (Self : in out Instance; Arg : in Packet_Id.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Product_Packetizer.Implementation;
