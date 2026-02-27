--------------------------------------------------------------------------------
-- Event_Filter Component Implementation Body
--------------------------------------------------------------------------------

with Event_Filter_Enums;
with Packet;
with Serializer_Types;

package body Component.Event_Filter.Implementation is
   use Event_Types;
   use Event_Filter_Entry;
   ---------------------------------------
   -- Protected Event_Filter_Entries Wrapper:
   ---------------------------------------
   -- This package is used only with this component and is wrapped as a protected object to protect the manipulation of entry data for each ID on its filtered state.
   protected body Protected_Event_Filter_Entries is
      procedure Init (Event_Id_Start : in Event_Types.Event_Id; Event_Id_Stop : in Event_Types.Event_Id; Event_Filter_List : in Event_Filter_Entry.Event_Id_List) is
      begin
         Event_Filter_Package.Init (Event_Id_Start, Event_Id_Stop, Event_Filter_List);
      end Init;

      procedure Destroy is
      begin
         Event_Filter_Package.Destroy;
      end Destroy;
      -- Procedure to set the filter state by command.
      procedure Set_Filter_State (Id : in Event_Types.Event_Id; New_State : in Event_Filter_State.E; Status : out Event_Filter_Entry.Event_Entry_Status) is
      begin
         Status := Event_Filter_Package.Set_Filter_State (Id, New_State);
      end Set_Filter_State;
      -- Procedure to determine if the event needs to be filtered and if so, reports that to the component for further handling.
      procedure Filter_Event (Id : in Event_Types.Event_Id; Status : out Event_Filter_Entry.Filter_Status) is
      begin
         Status := Event_Filter_Package.Filter_Event (Id);
      end Filter_Event;
      -- Procedure to set the global state of the component for filtering or not.
      procedure Set_Global_Enable_State (New_Global_State : in Global_Filter_State.E) is
      begin
         Event_Filter_Package.Set_Global_Enable_State (New_Global_State);
      end Set_Global_Enable_State;
      -- Procedure to get the global state of the component.
      function Get_Global_Enable_State return Global_Filter_State.E is
      begin
         return Event_Filter_Package.Get_Global_Enable_State;
      end Get_Global_Enable_State;
      -- Getters for filtered and unfiltered counts
      function Get_Event_Filtered_Count return Unsigned_32 is
      begin
         return Event_Filter_Package.Get_Event_Filtered_Count;
      end Get_Event_Filtered_Count;
      -- Getters for filtered and unfiltered counts
      function Get_Event_Unfiltered_Count return Unsigned_32 is
      begin
         return Event_Filter_Package.Get_Event_Unfiltered_Count;
      end Get_Event_Unfiltered_Count;
      -- Getter for maintaining the known range of IDs for the component to filter
      function Get_Event_Start_Stop_Range (Event_Stop_Id : out Event_Types.Event_Id) return Event_Types.Event_Id is
      begin
         return Event_Filter_Package.Get_Event_Start_Stop_Range (Event_Stop_Id);
      end Get_Event_Start_Stop_Range;
      -- Function to get the entry state array for packetizing
      function Get_Entry_Array return Basic_Types.Byte_Array_Access is
      begin
         return Event_Filter_Package.Get_Entry_Array;
      end Get_Entry_Array;

   end Protected_Event_Filter_Entries;

   --------------------------------------------------
   -- Subprogram for implementation Set_Up method:
   --------------------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set the component state and init the data products to 0
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Events_Filtered (Timestamp, ((Value => Self.Event_Entries.Get_Event_Filtered_Count))));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Events_Unfiltered (Timestamp, ((Value => Self.Event_Entries.Get_Event_Unfiltered_Count))));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Component_Filter_State (Timestamp, (Component_Filter_State => Self.Event_Entries.Get_Global_Enable_State)));
   end Set_Up;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Event_Id_Start_Range : Event_Types.Event_Id - The event ID that begins the range of ids that the component will include for filtering of events.
   -- Event_Id_End_Range : Event_Types.Event_Id - The event ID that ends the range of ids that the component will include for filtering of events.
   -- Event_Filter_List : Event_Filter_Entry.Event_Id_List - A list of event IDs that are filtered by default
   --
   overriding procedure Init (Self : in out Instance; Event_Id_Start_Range : in Event_Types.Event_Id; Event_Id_End_Range : in Event_Types.Event_Id; Event_Filter_List : in Event_Filter_Entry.Event_Id_List := [1 .. 0 => 0]) is
      Event_Entry_Array : Basic_Types.Byte_Array_Access;
   begin
      -- This is asserted in the package as well but added here for extra clarity
      pragma Assert (Event_Id_Start_Range <= Event_Id_End_Range, "End id must be equal to or greater than the start ID for the event filter");
      Self.Event_Entries.Init (Event_Id_Start_Range, Event_Id_End_Range, Event_Filter_List);

      -- Grab the array length of the entries to make sure that the length isn't longer than the max size of a packet
      Event_Entry_Array := Self.Event_Entries.Get_Entry_Array;
      -- Make sure our size is not larger than the size of a packet
      pragma Assert (Event_Entry_Array'Length <= Packet_Types.Packet_Buffer_Length_Type'Last, "Packet length for the event states in event filter is larger than the max packet length");
      -- Finally set our protected variable
      Self.Send_Event_State_Packet.Set_Var (False);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Upon reception the component will record the number of events that have been filtered and send the state packet if it was requested.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Num_Events_Filtered : constant Unsigned_32 := Self.Event_Entries.Get_Event_Filtered_Count;
      Num_Events_Unfiltered : constant Unsigned_32 := Self.Event_Entries.Get_Event_Unfiltered_Count;
      -- Grab the protected variable for the packet and for the global enable status
      Send_Packet : constant Boolean := Self.Send_Event_State_Packet.Get_Var;

   begin
      -- If the number of events filtered is not what it was previously, then update the total number of events filtered for the component lifetime and reset the number since last tick count
      if Num_Events_Filtered /= Self.Total_Event_Filtered_Count then
         Self.Total_Event_Filtered_Count := Num_Events_Filtered;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Events_Filtered (Self.Sys_Time_T_Get, ((Value => Self.Total_Event_Filtered_Count))));
      end if;

      -- If the number of events unfiltered is not what it was previously, then update the total number of events unfiltered for the component lifetime and reset the number since last tick count
      if Num_Events_Unfiltered /= Self.Total_Event_Unfiltered_Count then
         Self.Total_Event_Unfiltered_Count := Num_Events_Unfiltered;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Events_Unfiltered (Self.Sys_Time_T_Get, ((Value => Self.Total_Event_Unfiltered_Count))));
      end if;

      -- Check to see if we need to send the state packet
      if Send_Packet then
         declare
            use Serializer_Types;
            Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
            Packet_Bytes : Basic_Types.Byte_Array_Access;
            State_Packet_Status : Serialization_Status;
            State_Packet : Packet.T;
         begin
            -- Grab the bytes from the package. As a note, this is not thread safe, but is done for speed. It is assumed that we will not be changing states while the packet is being sent.
            Packet_Bytes := Self.Event_Entries.Get_Entry_Array;
            -- Send out the packet:
            State_Packet_Status := Self.Packets.Event_Filter_State_Packet (Timestamp, Packet_Bytes.all, State_Packet);
            pragma Assert (State_Packet_Status = Success, "Too many states for the size of a packet!");
            Self.Packet_T_Send_If_Connected (State_Packet);
            -- Sent the packet so set the flag back to false
            Self.Send_Event_State_Packet.Set_Var (False);
         end;

      end if;
   end Tick_T_Recv_Sync;

   -- Events are received synchronously on this connector and are passed along or filtered.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
      Status : Event_Filter_Entry.Filter_Status;
   begin
      -- Look up the event in the package to determine if the event needs to be forwarded
      Self.Event_Entries.Filter_Event (Arg.Header.Id, Status);

      case Status is
         -- When the id is not filtered or not in our range, then pass it on
         when Unfiltered | Out_Of_Range =>
            Self.Event_Forward_T_Send_If_Connected (Arg);
            -- When Filtered, do nothing
         when Filtered =>
            null;
      end case;
   end Event_T_Recv_Sync;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the event filter component.
   -- Enable the event filter for a specific event ID.
   overriding function Filter_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Filter_Enums;
      Status : Event_Filter_Entry.Event_Entry_Status;
      Ret : Command_Execution_Status.E := Success;
   begin
      Self.Event_Entries.Set_Filter_State (Arg.Event_To_Update.Id, Event_Filter_State.Filtered, Status);
      case Status is
         when Invalid_Id =>
            Self.Event_T_Send_If_Connected (Self.Events.Filter_Event_Invalid_Id (Self.Sys_Time_T_Get, Arg));
            return Failure;
         when Success =>
            Self.Event_T_Send_If_Connected (Self.Events.Filtered_Event (Self.Sys_Time_T_Get, Arg));
      end case;

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Filter_Event;

   -- Disable the event filter for a specific event ID.
   overriding function Unfilter_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Filter_Enums;
      Status : Event_Filter_Entry.Event_Entry_Status;
      Ret : Command_Execution_Status.E := Success;
   begin
      Self.Event_Entries.Set_Filter_State (Arg.Event_To_Update.Id, Event_Filter_State.Unfiltered, Status);
      case Status is
         when Invalid_Id =>
            Self.Event_T_Send_If_Connected (Self.Events.Unfilter_Event_Invalid_Id (Self.Sys_Time_T_Get, Arg));
            return Failure;
         when Success =>
            Self.Event_T_Send_If_Connected (Self.Events.Unfiltered_Event (Self.Sys_Time_T_Get, Arg));
      end case;

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Unfilter_Event;

   -- Enable the event filter for a specific range of event IDs.
   overriding function Filter_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Filter_Enums;
      Status : Event_Filter_Entry.Event_Entry_Status;
      Ret : Command_Execution_Status.E := Success;
      Id_Stop : Event_Types.Event_Id;
      Id_Start : constant Event_Types.Event_Id := Self.Event_Entries.Get_Event_Start_Stop_Range (Id_Stop);
   begin
      -- Make sure the range of ids will not be invalid before attempting to change all in the range
      if Id_Start <= Arg.Start_Event_Id.Id and then Id_Stop >= Arg.Stop_Event_Id.Id and then Arg.Start_Event_Id.Id <= Arg.Stop_Event_Id.Id then
         for Event_Id_State_Change in Arg.Start_Event_Id.Id .. Arg.Stop_Event_Id.Id loop
            Self.Event_Entries.Set_Filter_State (Event_Id_State_Change, Event_Filter_State.Filtered, Status);
            case Status is
               when Invalid_Id =>
                  pragma Assert (False, "Found Invalid_Id for the Event Filter when commanding enable range of events, which should have been caught in an earlier statement");
               when Success =>
                  null; -- expected so continue to loop and do nothing in this case
            end case;
         end loop;
      else -- The provided event id's were out of range here.
         Self.Event_T_Send_If_Connected (Self.Events.Filter_Event_Range_Invalid_Id (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;
      Self.Event_T_Send_If_Connected (Self.Events.Filtered_Event_Range (Self.Sys_Time_T_Get, Arg));

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Filter_Event_Range;

   -- Disable the event filter for a specific range of event IDs.
   overriding function Unfilter_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Filter_Enums;
      Status : Event_Filter_Entry.Event_Entry_Status;
      Ret : Command_Execution_Status.E := Success;
      Id_Stop : Event_Types.Event_Id;
      Id_Start : constant Event_Types.Event_Id := Self.Event_Entries.Get_Event_Start_Stop_Range (Id_Stop);
   begin
      -- Make sure the range of ids will not be invalid before attempting to change all in the range
      if Id_Start <= Arg.Start_Event_Id.Id and then Id_Stop >= Arg.Stop_Event_Id.Id and then Arg.Start_Event_Id.Id <= Arg.Stop_Event_Id.Id then
         for Event_Id_State_Change in Arg.Start_Event_Id.Id .. Arg.Stop_Event_Id.Id loop
            Self.Event_Entries.Set_Filter_State (Event_Id_State_Change, Event_Filter_State.Unfiltered, Status);
            case Status is
               when Invalid_Id =>
                  pragma Assert (False, "Found Invalid_Id for the Event Filter when commanding enable range of events, which should have been caught in an earlier statement");
               when Success =>
                  null; -- expected so continue to loop and do nothing in this case
            end case;
         end loop;
      else -- The provided event id's were out of range here.
         Self.Event_T_Send_If_Connected (Self.Events.Unfilter_Event_Range_Invalid_Id (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;
      Self.Event_T_Send_If_Connected (Self.Events.Unfiltered_Event_Range (Self.Sys_Time_T_Get, Arg));

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Unfilter_Event_Range;

   -- Enable the component to filter events that have been set to be filtered.
   overriding function Enable_Event_Filtering (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- All we need to do here is enable the component level variable
      Self.Event_Entries.Set_Global_Enable_State (Global_Filter_State.Enabled);
      Self.Event_T_Send_If_Connected (Self.Events.Enable_Event_Filter (Timestamp));
      -- Update the data product as well
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Component_Filter_State (Timestamp, (Component_Filter_State => Self.Event_Entries.Get_Global_Enable_State)));

      return Success;
   end Enable_Event_Filtering;

   -- Disable the component so that all events will not be filtered. The event states will be maintained for when re-enabled.
   overriding function Disable_Event_Filtering (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- All we need to do here is disable the component level variable
      Self.Event_Entries.Set_Global_Enable_State (Global_Filter_State.Disabled);
      Self.Event_T_Send_If_Connected (Self.Events.Disable_Event_Filter (Timestamp));
      -- Update the data product as well
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Component_Filter_State (Timestamp, (Component_Filter_State => Self.Event_Entries.Get_Global_Enable_State)));

      return Success;
   end Disable_Event_Filtering;

   -- Dump a packet for the state of all events on if they are filtered or not.
   overriding function Dump_Event_States (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Send_Event_State_Packet.Set_Var (True);
      Self.Event_T_Send_If_Connected (Self.Events.Dump_Event_States_Received (Self.Sys_Time_T_Get));
      return Success;
   end Dump_Event_States;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field
      )));
   end Invalid_Command;

end Component.Event_Filter.Implementation;
