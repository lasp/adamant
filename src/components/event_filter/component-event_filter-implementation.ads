--------------------------------------------------------------------------------
-- Event_Filter Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Event;
with Command;
with Event_Filter_Entry;
with Event_Filter_Entry_Enums;
with Protected_Variables;

-- The Event Filter component is used to filter out event IDs from leaving the system. The component takes in a range of IDs
package Component.Event_Filter.Implementation is
   use Event_Filter_Entry_Enums;
   -- The component class instance record:
   type Instance is new Event_Filter.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Event_Id_Start_Range : Event_Types.Event_Id - The event ID that begins the range of ids that the component will include for filtering of events.
   -- Event_Id_End_Range : Event_Types.Event_Id - The event ID that ends the range of ids that the component will include for filtering of events.
   -- Event_Filter_List : Event_Filter_Entry.Event_Id_List - A list of event IDs that are filtered by default
   --
   overriding procedure Init (Self : in out Instance; Event_Id_Start_Range : in Event_Types.Event_Id; Event_Id_End_Range : in Event_Types.Event_Id; Event_Filter_List : in Event_Filter_Entry.Event_Id_List := [1 .. 0 => 0]);

private

   -- Protected boolean for sending the state packet
   package Protected_Boolean is new Protected_Variables.Generic_Variable (Boolean);

   protected type Protected_Event_Filter_Entries is
      -- Procedures requiring full mutual exclusion:
      -- This package is used only with this component and is wrapped as a protected object to protect the manipulation of entry data for each ID on its filtered state.
      procedure Init (Event_Id_Start : in Event_Types.Event_Id; Event_Id_Stop : in Event_Types.Event_Id; Event_Filter_List : in Event_Filter_Entry.Event_Id_List);
      procedure Destroy;
      -- Procedure to set the filter state by command.
      procedure Set_Filter_State (Id : in Event_Types.Event_Id; New_State : in Event_Filter_State.E; Status : out Event_Filter_Entry.Event_Entry_Status);
      -- Procedure to determine if the event needs to be filtered and if so, reports that to the component for further handling.
      procedure Filter_Event (Id : in Event_Types.Event_Id; Status : out Event_Filter_Entry.Filter_Status);
      -- Procedure to set the global state of the component for filtering or not.
      procedure Set_Global_Enable_State (New_Global_State : in Global_Filter_State.E);
      -- Procedure to get the global state of the component.
      function Get_Global_Enable_State return Global_Filter_State.E;
      -- Getters for filtered and unfiltered counts
      function Get_Event_Filtered_Count return Unsigned_32;
      function Get_Event_Unfiltered_Count return Unsigned_32;
      -- Getter for maintaining the known range of IDs for the component to filter
      function Get_Event_Start_Stop_Range (Event_Stop_Id : out Event_Types.Event_Id) return Event_Types.Event_Id;
      -- Function to get the entry state array for packetizing
      function Get_Entry_Array return Basic_Types.Byte_Array_Access;

   private
      Event_Filter_Package : Event_Filter_Entry.Instance;
   end Protected_Event_Filter_Entries;

   -- The component class instance record:
   type Instance is new Event_Filter.Base_Instance with record
      Event_Entries : Protected_Event_Filter_Entries;
      -- Packet variables for sending the state packet
      Send_Event_State_Packet : Protected_Boolean.Variable;
      -- Event Filter count for lifetime of component. Tracked in the package, and a copy is stored here so that we don't send the data product if we don't have to
      Total_Event_Filtered_Count : Unsigned_32 := Unsigned_32'First;
      -- Event unfiltered count for lifetime of component. Tracked in the package, and a copy is stored here so that we don't send the data product if we don't have to. Does not include invalid id counts.
      Total_Event_Unfiltered_Count : Unsigned_32 := Unsigned_32'First;
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
   -- This is the base tick for the component. Upon reception the component will record the number of events that have been filtered and send the state packet if it was requested.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Events are received synchronously on this connector and are passed along or filtered.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_Forward_T_Send message is dropped due to a full queue.
   overriding procedure Event_Forward_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
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
   --    These are the commands for the event filter component.
   -- Enable the event filter for a specific event ID.
   overriding function Filter_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) return Command_Execution_Status.E;
   -- Disable the event filter for a specific event ID.
   overriding function Unfilter_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) return Command_Execution_Status.E;
   -- Enable the event filtering for a specific range of event IDs.
   overriding function Filter_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) return Command_Execution_Status.E;
   -- Disable the event filtering for a specific range of event IDs.
   overriding function Unfilter_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) return Command_Execution_Status.E;
   -- Enable the component to filter events that have been set to be filtered.
   overriding function Enable_Event_Filtering (Self : in out Instance) return Command_Execution_Status.E;
   -- Disable the component so that all events will not be filtered. The event states will be maintained for when re-enabled.
   overriding function Disable_Event_Filtering (Self : in out Instance) return Command_Execution_Status.E;
   -- Dump a packet for the state of all events pertaining to if they are filtered or not.
   overriding function Dump_Event_States (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Event_Filter.Implementation;
