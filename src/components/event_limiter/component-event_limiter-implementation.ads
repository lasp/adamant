--------------------------------------------------------------------------------
-- Event_Limiter Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Event;
with Command;
with Protected_Variables;
with Two_Counter_Entry;
with Two_Counter_Entry_Enums;

-- The Event Limiter takes in events and checks that there have not been too many events of a single ID within one tick that would cause the system to flood with events. Every tick the event counts are decremented to keep events flowing at the appropriate rate. The component takes in a start and stop ID and should include all IDs in that range. The component also takes a list of event ID's to set as disabled by default. All others will be enabled by default.
package Component.Event_Limiter.Implementation is
   use Two_Counter_Entry_Enums;
   -- The component class instance record:
   type Instance is new Event_Limiter.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Event_Id_Start : Event_Types.Event_Id - The event ID that begins the range of ids that the component will include for potential limiting of events.
   -- Event_Id_Stop : Event_Types.Event_Id - The event ID that ends the range of ids that the component will include for potential limiting of events.
   -- Event_Disable_List : Two_Counter_Entry.Event_Id_List - A list of event IDs that are disabled by default
   -- Event_Limit_Persistence : Two_Counter_Entry.Persistence_Type - The initial persistence of the number of events to allow before limiting them between ticks (1 to 7)
   --
   overriding procedure Init
      (Self : in out Instance; Event_Id_Start : in Event_Types.Event_Id; Event_Id_Stop : in Event_Types.Event_Id; Event_Disable_List : in Two_Counter_Entry.Event_Id_List := [1 .. 0 => 0]; Event_Limit_Persistence : in Two_Counter_Entry.Persistence_Type);

private

   -- Protected boolean for sending the state packet
   package Protected_Boolean is new Protected_Variables.Generic_Variable (Boolean);

   -- Type to help when we fill in the state packet. Keeps track of which bit we are currently filling into the packet and keeps the case statement cleaner
   type Bit_Num is mod 8;

   protected type Protected_Two_Counter_Entry is
      -- Procedures requiring full mutual exclusion:
      procedure Init (Event_Id_Start : in Event_Types.Event_Id; Event_Id_Stop : in Event_Types.Event_Id; Event_Disable_List : in Two_Counter_Entry.Event_Id_List; Event_Limit_Persistence : in Two_Counter_Entry.Persistence_Type);
      procedure Destroy;
      procedure Increment_Counter (Id : in Event_Types.Event_Id; Status : out Two_Counter_Entry.Count_Status);
      procedure Decrement_Counter (Id : in Event_Types.Event_Id; Status : out Two_Counter_Entry.Count_Status);
      procedure Set_Enable_State (Id : in Event_Types.Event_Id; New_State : in Event_State_Type.E; Status : out Two_Counter_Entry.Enable_State_Status);
      function Get_Enable_State (Id : in Event_Types.Event_Id; Event_State : out Event_State_Type.E) return Two_Counter_Entry.Enable_State_Status;
      procedure Set_Persistence (New_Persistence : in Two_Counter_Entry.Persistence_Type);
      function Get_Persistence return Two_Counter_Entry.Persistence_Type;
      procedure Set_Master_Enable_State (New_Master_State : in Event_State_Type.E);
      function Get_Master_Enable_State return Event_State_Type.E;
      function Get_Events_Limited_Count return Unsigned_16;
      procedure Reset_Event_Limited_Count;
      function Get_Event_Start_Stop_Range (Event_Stop_Id : out Event_Types.Event_Id) return Event_Types.Event_Id;

   private
      Event_Counter_Package : Two_Counter_Entry.Instance;
   end Protected_Two_Counter_Entry;

   -- The component class instance record:
   type Instance is new Event_Limiter.Base_Instance with record
      Event_Array : Protected_Two_Counter_Entry;
      -- Packet variables for the state packet
      Send_Event_State_Packet : Protected_Boolean.Variable;
      State_Packet_Size : Natural := Natural'First;
      -- Event limited count for lifetime of component
      Total_Event_Limited_Count : Unsigned_32 := Unsigned_32'First;
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
   -- This is the base tick for the component. Upon reception the component will decrement the count of each ID unless it is already 0. Every 10 ticks, an event of what is filtered will be sent.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Events are received synchronously on this connector and checked for the number of events of that ID.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Event_Forward_T_Send message is dropped due to a full queue.
   overriding procedure Event_Forward_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the event limiter component.
   -- Enable the event limiter for a specific event ID.
   overriding function Enable_Event_Limit (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) return Command_Execution_Status.E;
   -- Disable the event limiter for a specific event ID.
   overriding function Disable_Event_Limit (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) return Command_Execution_Status.E;
   -- Enable the event limiter for a specific range of event ID.
   overriding function Enable_Event_Limit_Range (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) return Command_Execution_Status.E;
   -- Disable the event limiter for a specific range of event ID.
   overriding function Disable_Event_Limit_Range (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) return Command_Execution_Status.E;
   -- Enable the event limiters for all event IDs.
   overriding function Enable_Event_Limiting (Self : in out Instance) return Command_Execution_Status.E;
   -- Disable the event limiters for all event IDs.
   overriding function Disable_Event_Limiting (Self : in out Instance) return Command_Execution_Status.E;
   -- Set the persistence of the event limiter for all events that are limited. Value must be between 0 and 7.
   overriding function Set_Event_Limit_Persistence (Self : in out Instance; Arg : in Event_Limiter_Persistence_Type.T) return Command_Execution_Status.E;
   -- Dump a packet for the state of all events on if they are limited or not.
   overriding function Dump_Event_States (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Event_Limiter.Implementation;
