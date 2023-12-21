--------------------------------------------------------------------------------
-- Event_Packetizer Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Tick;
with Event;
with Command;

-- The Event Packetizer component receives events synchronously and places them into a packet. This component receives a periodic tick. A packet is sent out upon receiving a tick if 1) the component has a full packet to send or 2) a partial packet timeout has occurred and the component has a packet with at least one event in it.
--
package Component.Event_Packetizer.Implementation is

   -- The component class instance record:
   type Instance is new Event_Packetizer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   overriding procedure Init (Self : in out Instance; Num_Internal_Packets : in Two_Or_More; Partial_Packet_Timeout : in Natural);
   not overriding procedure Destroy (Self : in out Instance);

private

   ---------------------------------------
   -- Internal protected type:
   ---------------------------------------

   -- Packet array types:
   subtype Packet_Array_Index is Positive;
   type Packet_Array is array (Packet_Array_Index range <>) of Packet.T;
   type Packet_Array_Access is access Packet_Array;

   -- Status return types for protected object:
   type Insert_Status is (Success, Packets_Full, Uninitialized);
   type Pop_Status is (Success, Packet_Empty, Packet_Partially_Full);
   type Dropped_Count_Status is (Same_As_Last_Request, Increased_Since_Last_Request);

   -- Protected object for providing mutual exclusion to internal packet
   -- data structure:
   protected type Protected_Packet_Array is
      -- Procedures requiring full mutual exclusion:
      procedure Init (Num_Internal_Packets : in Two_Or_More);
      procedure Destroy;
      procedure Insert_Event (Evt : in Event.T; Status : out Insert_Status);
      procedure Pop_Packet (The_Packet : out Packet.T; Allow_Partial_Packet : in Boolean; Status : out Pop_Status);
      procedure Get_Dropped_Count (Count : out Unsigned_32; Status : out Dropped_Count_Status);
      function Get_Bytes_Available return Natural;
   private
      -- Internal packets:
      Packets : Packet_Array_Access;
      -- Current packet index:
      Index : Packet_Array_Index := Packet_Array_Index'First;
      Num_Packets_Full : Natural := 0;
      -- Current dropped event count:
      Events_Dropped : Unsigned_32 := 0;
      New_Packets_Dropped : Boolean := False;
      -- Has the array been initialized. Until it has, drop all incoming packets:
      Initialized : Boolean := False;
   end Protected_Packet_Array;

   ---------------------------------------
   -- Component Instance record:
   ---------------------------------------

   -- The component class instance record:
   type Instance is new Event_Packetizer.Base_Instance with record
      -- Internal packets:
      Packet_Array : Protected_Packet_Array;
      -- Partial packet count - how many ticks have passed while component
      -- has held a partial packet.
      Partial_Packet_Count : Natural := 0;
      Partial_Packet_Timeout : Natural := 0;
      -- Boolean to send command next tick:
      Send_Packet_Next_Tick : Boolean := False;
      Previous_Bytes_Available : Natural := Natural'Last;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Upon reception the component will send out one full packet, if a full packet is contained within the component. A partial packet will be sent out if the packet timeout occurs.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Events are received synchronously on this connector and stored into an internal packet.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the event packetizer component.
   -- Send a packet out next tick, unless there are no events stored within the component.
   overriding function Send_Packet (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Event_Packetizer.Implementation;
