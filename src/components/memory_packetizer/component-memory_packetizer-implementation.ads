--------------------------------------------------------------------------------
-- Memory_Packetizer Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Memory_Packetizer_Types;
with Command;
with Packet_Types;
with Ada.Real_Time;

-- This active component receives memory pointer information on an asynchronous queue. It then reads the data that these pointers reference into packets, producing multiple maximum sized packets, if necessary, to packetize the entire memory region.
package Component.Memory_Packetizer.Implementation is

   -- The component class instance record:
   type Instance is new Memory_Packetizer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to set a threshold for the maximum number of packets that the component will produce in a single time period. A time period is measured in an integer number of seconds. The component also needs to keep track of the sequence counts for each packet ID that it receives. To do this, it needs to allocate internal memory to keep track of the last sequence count for each packet. A maximum number of unique packet ids is provided in this function to allocate the necessary memory to keep track of this information.
   --
   -- Init Parameters:
   -- Max_Packets_Per_Time_Period : Natural - The maximum number of packets that this component will produce in a single second. The component will stop producing packets if the threshold is met, until the end of a second period has elapsed.
   -- Time_Period_In_Seconds : Positive - The time period in seconds over which to measure the number of packets produced.
   -- Max_Packet_Ids : Positive - The maximum number of unique packet ids that this component is expected to receive during operations. This value is used to allocate a small amount of memory at initialization to keep track of the sequence count for each produced packet. If this memory becomes all used up, any new unique packet ids received will trigger an event and will be emitted with a sequence count of zero. This misconfiguration should easily be detectable during test.
   --
   overriding procedure Init (Self : in out Instance; Max_Packets_Per_Time_Period : in Natural; Time_Period_In_Seconds : in Positive := 1; Max_Packet_Ids : in Positive := 10);
   not overriding procedure Final (Self : in out Instance);

private

   -- Record which holds the current sequence number for a given packet id:
   type Sequence_Count_Tracker is record
      Id : Packet_Types.Packet_Id;
      Sequence_Count : Packet_Types.Sequence_Count_Mod_Type := 0;
      Used : Boolean := False;
   end record;

   -- List of sequence number tracker records:
   type Sequence_Count_Tracker_List is array (Positive range <>) of Sequence_Count_Tracker;
   type Sequence_Count_Tracker_List_Access is access all Sequence_Count_Tracker_List;

   -- The component class instance record:
   type Instance is new Memory_Packetizer.Base_Instance with record
      -- A list of sequence count trackers.
      Sequence_Count_List : Sequence_Count_Tracker_List_Access := null;
      -- The length of the time period.
      Time_Period_S : Positive; -- in seconds
      Time_Period : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1_000);
      -- Time denoting the beginning of the next time period.
      Next_Period_Start : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      -- The maximum number of packets that can be sent in a single time period.
      Max_Packets_Per_Time_Period : Natural := 1;
      -- The number of packets sent in this time period.
      Num_Packets_Sent : Natural := 0;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- A memory dump pointer and id queued up for packetization on this connector.
   overriding procedure Memory_Dump_Recv_Async (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump);
   -- This procedure is called when a Memory_Dump_Recv_Async message is dropped due to a full queue.
   overriding procedure Memory_Dump_Recv_Async_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is null;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the memory packetizer component.
   -- Set a new value for the Max_Packets_Per_Time_Period and the Time_Period_In_Seconds to control the output rate of the emitted packets.
   overriding function Set_Max_Packet_Rate (Self : in out Instance; Arg : in Packets_Per_Period.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Memory_Packetizer.Implementation;
