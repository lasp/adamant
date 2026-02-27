--------------------------------------------------------------------------------
-- Event_Limiter Component Implementation Body
--------------------------------------------------------------------------------

with Event_Id_Limiter_State_Type;
with Event_Limiter_Num_Events_Type;
with Packet;
with Packet_Types;
with Serializer_Types;
with Event_Limiter_Enums;

package body Component.Event_Limiter.Implementation is
   use Event_Types;
   use Two_Counter_Entry;
   ---------------------------------------
   -- Protected Two_Counter_Entry Wrapper:
   ---------------------------------------
   protected body Protected_Two_Counter_Entry is
      procedure Init (Event_Id_Start : in Event_Types.Event_Id; Event_Id_Stop : in Event_Types.Event_Id; Event_Disable_List : in Two_Counter_Entry.Event_Id_List; Event_Limit_Persistence : in Two_Counter_Entry.Persistence_Type) is
      begin
         Event_Counter_Package.Init (Event_Id_Start, Event_Id_Stop, Event_Disable_List, Event_Limit_Persistence);
      end Init;

      procedure Destroy is
      begin
         Event_Counter_Package.Destroy;
      end Destroy;

      procedure Increment_Counter (Id : in Event_Types.Event_Id; Status : out Two_Counter_Entry.Count_Status) is
      begin
         Status := Event_Counter_Package.Increment_Counter (Id);
      end Increment_Counter;

      procedure Decrement_Counter (Id : in Event_Types.Event_Id; Status : out Two_Counter_Entry.Count_Status) is
      begin
         Status := Event_Counter_Package.Decrement_Counter (Id);
      end Decrement_Counter;

      procedure Set_Enable_State (Id : in Event_Types.Event_Id; New_State : in Event_State_Type.E; Status : out Two_Counter_Entry.Enable_State_Status) is
      begin
         Status := Event_Counter_Package.Set_Enable_State (Id, New_State);
      end Set_Enable_State;

      function Get_Enable_State (Id : in Event_Types.Event_Id; Event_State : out Event_State_Type.E) return Two_Counter_Entry.Enable_State_Status is
      begin
         return Event_Counter_Package.Get_Enable_State (Id, Event_State);
      end Get_Enable_State;

      procedure Set_Persistence (New_Persistence : in Two_Counter_Entry.Persistence_Type) is
      begin
         Event_Counter_Package.Set_Persistence (New_Persistence);
      end Set_Persistence;

      function Get_Persistence return Two_Counter_Entry.Persistence_Type is
      begin
         return Event_Counter_Package.Get_Persistence;
      end Get_Persistence;

      procedure Set_Master_Enable_State (New_Master_State : in Event_State_Type.E) is
      begin
         Event_Counter_Package.Set_Master_Enable_State (New_Master_State);
      end Set_Master_Enable_State;

      function Get_Master_Enable_State return Event_State_Type.E is
      begin
         return Event_Counter_Package.Get_Master_Enable_State;
      end Get_Master_Enable_State;

      function Get_Events_Limited_Count return Unsigned_16 is
      begin
         return Event_Counter_Package.Get_Events_Limited_Count;
      end Get_Events_Limited_Count;

      procedure Reset_Event_Limited_Count is
      begin
         Event_Counter_Package.Reset_Event_Limited_Count;
      end Reset_Event_Limited_Count;

      function Get_Event_Start_Stop_Range (Event_Stop_Id : out Event_Types.Event_Id) return Event_Types.Event_Id is
      begin
         return Event_Counter_Package.Get_Event_Start_Stop_Range (Event_Stop_Id);
      end Get_Event_Start_Stop_Range;

   end Protected_Two_Counter_Entry;

   --------------------------------------------------
   -- Subprogram for implementation Set_Up method:
   --------------------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set the component state and init the data products to 0
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Limited_Events_Since_Tick (Timestamp, ((Value => 0))));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Events_Limited (Timestamp, ((Value => 0))));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Component_Limiting_Enabled_Status (Timestamp, (Component_Enable_State => Self.Event_Array.Get_Master_Enable_State)));
   end Set_Up;
   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Event_Id_Start : Event_Types.Event_Id - The event ID that begins the range of ids that the component will include for potential limiting of events.
   -- Event_Id_Stop : Event_Types.Event_Id - The event ID that ends the range of ids that the component will include for potential limiting of events.
   -- Event_Disable_List : Two_Counter_Entry.Event_Id_List - A list of event IDs that are enabled by default
   -- Event_Limit_Persistence : Two_Counter_Entry.Persistence_Type - The initial persistence of the number of events to allow before limiting them between ticks (1 to 7)
   --
   overriding procedure Init
      (Self : in out Instance; Event_Id_Start : in Event_Types.Event_Id; Event_Id_Stop : in Event_Types.Event_Id; Event_Disable_List : in Two_Counter_Entry.Event_Id_List := [1 .. 0 => 0]; Event_Limit_Persistence : in Two_Counter_Entry.Persistence_Type)
   is
   begin
      Self.Event_Array.Init (Event_Id_Start, Event_Id_Stop, Event_Disable_List, Event_Limit_Persistence);
      -- This is asserted in the package as well but added here for extra clarity
      pragma Assert (Event_Id_Stop >= Event_Id_Start, "Stop id must be equal to or greater than the start ID for the event limiter");

      -- Divide by 8 to determine the number of ids we need to accommodate (1 id per bit). Add eight since the difference needs to account for an extra byte if we have ids that need to fil up some of the next byte
      Self.State_Packet_Size := Natural (Event_Id_Stop - Event_Id_Start + 8) / 8;
      -- Make sure our size is not larger than the size of a packet
      pragma Assert (Self.State_Packet_Size <= Packet_Types.Packet_Buffer_Length_Type'Last, "Packet length for the event states in event limiter is larger than the max packet length");

      -- Initialize our flag to false which indicates if we should be sending the packet next tick
      Self.Send_Event_State_Packet.Set_Var (False);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Upon reception the component will decrement the count of each ID unless it is already 0. Every 10 ticks, an event of what is filtered will be sent.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Event_State_Type;
      -- Get the timestamp for the package
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Status : Two_Counter_Entry.Count_Status;
      Num_Events_Limited : constant Unsigned_16 := Self.Event_Array.Get_Events_Limited_Count;
      -- Grab the protected variable for the packet and for the global enable status
      Send_Packet : constant Boolean := Self.Send_Event_State_Packet.Get_Var;
      Component_State : constant Event_State_Type.E := Self.Event_Array.Get_Master_Enable_State;
      -- Event to indicate the number of events limited and a set of ID's
      Num_Event_Limited_Event : Event_Limiter_Num_Events_Type.T := (Num_Event_Ids => 0, Num_Events_Limited => 0, Event_Id_Limited_Array => [others => 0]);
      Id_Stop : Event_Types.Event_Id;
      Id_Start : constant Event_Types.Event_Id := Self.Event_Array.Get_Event_Start_Stop_Range (Id_Stop);
   begin
      -- First, determine if the component is disabled and skip the decrement if so.
      if Component_State = Enabled then
         -- Decrement all IDs here from the start id to the stop
         for Dec_Event_Id in Id_Start .. Id_Stop loop
            Self.Event_Array.Decrement_Counter (Dec_Event_Id, Status);
            case Status is
               when Success =>
                  null; -- do nothing here and move on
                  -- Save the event id into our event that indicates this id was limited (if room is available)
               when Event_Max_Limit =>
                  if Num_Event_Limited_Event.Num_Event_Ids <= Num_Event_Limited_Event.Event_Id_Limited_Array'Length then
                     Num_Event_Limited_Event.Event_Id_Limited_Array (Integer (Num_Event_Limited_Event.Num_Event_Ids)) := Dec_Event_Id;
                     Num_Event_Limited_Event.Num_Event_Ids := @ + 1;
                  end if;
                  -- Assert on the status. We know the range so we shouldn't get an Invalid_Id error
               when Invalid_Id =>
                  pragma Assert (False, "Invalid_Id found when decrementing all event limiter counters which should not be possible: " & Natural'Image (Natural (Dec_Event_Id)));
            end case;
         end loop;
      end if;

      -- If we have been told to dump a packet, then loop through all and get the state to pack into the packet.
      if Send_Packet then
         declare
            use Serializer_Types;
            -- Byte array for the packet
            Event_State_Array : Basic_Types.Byte_Array (0 .. Self.State_Packet_Size - 1) := [others => 16#FF#];
            State_Packet : Packet.T;
            State_Packet_Status : Serialization_Status;
            Byte_Num : Natural := 0;
            Bit_Location : Bit_Num := Bit_Num'First;
            Event_Bitmap : Event_Id_Limiter_State_Type.T;
            Event_Status : Two_Counter_Entry.Enable_State_Status;
            Event_State : Event_State_Type.E;
         begin
            for Id in Id_Start .. Id_Stop loop
               Event_Status := Self.Event_Array.Get_Enable_State (Id, Event_State);
               case Event_Status is
                  when Success =>
                     -- Using the Bit_Num type to enforce that we don't go past 8 and keep the case statement cleaner
                     Bit_Location := Bit_Num ((Id - Id_Start) mod 8);
                     case Bit_Location is
                        when 0 =>
                           -- Reset the bitmap in case this is the last byte but isn't fully filled
                           Event_Bitmap := (State_0 => Disabled, State_1 => Disabled, State_2 => Disabled, State_3 => Disabled, State_4 => Disabled, State_5 => Disabled, State_6 => Disabled, State_7 => Disabled);
                           Event_Bitmap.State_0 := Event_State;
                        when 1 =>
                           Event_Bitmap.State_1 := Event_State;
                        when 2 =>
                           Event_Bitmap.State_2 := Event_State;
                        when 3 =>
                           Event_Bitmap.State_3 := Event_State;
                        when 4 =>
                           Event_Bitmap.State_4 := Event_State;
                        when 5 =>
                           Event_Bitmap.State_5 := Event_State;
                        when 6 =>
                           Event_Bitmap.State_6 := Event_State;
                        when 7 =>
                           Event_Bitmap.State_7 := Event_State;
                           Event_State_Array (Byte_Num) := Event_Id_Limiter_State_Type.Serialization.To_Byte_Array (Event_Bitmap) (0);
                           Byte_Num := @ + 1;
                     end case;
                  when Invalid_Id =>
                     pragma Assert (False, "Invalid_Id found when decrementing all event limiter counters which should not be possible: " & Natural'Image (Natural (Id)));
               end case;
            end loop;
            -- Finish the last byte of the packet if necessary
            if Bit_Location /= 7 then
               Event_State_Array (Byte_Num) := Event_Id_Limiter_State_Type.Serialization.To_Byte_Array (Event_Bitmap) (0);
            end if;
            -- Send out the packet:
            State_Packet_Status := Self.Packets.Event_Limiter_State_Packet (Timestamp, Event_State_Array, State_Packet);
            pragma Assert (State_Packet_Status = Success, "Too many states for the size of a packet!");
            Self.Packet_T_Send_If_Connected (State_Packet);
            -- Sent the packet so set the flag back to false
            Self.Send_Event_State_Packet.Set_Var (False);
         end;
      end if; -- end if send_packet

      -- Data products and Limited number of events, event:
      -- Send the number of events limited since last tick because we never know what the previous amount was
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Limited_Events_Since_Tick (Timestamp, ((Value => Num_Events_Limited))));

      -- If we have events that were limited, then send the event indicating such, but don't if there is nothing to send
      if Num_Event_Limited_Event.Num_Event_Ids > 0 then
         Num_Event_Limited_Event.Num_Events_Limited := Num_Events_Limited;
         Self.Event_T_Send_If_Connected (Self.Events.Events_Limited_Since_Last_Tick (Timestamp, Num_Event_Limited_Event));
      end if;
      -- If the number of events limited is not 0, then update the total number of events limited for the component lifetime and reset the number since last tick count
      if Num_Events_Limited > 0 then
         Self.Total_Event_Limited_Count := @ + Unsigned_32 (Num_Events_Limited);
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Events_Limited (Timestamp, ((Value => Self.Total_Event_Limited_Count))));

         Self.Event_Array.Reset_Event_Limited_Count;
      end if;

   end Tick_T_Recv_Sync;

   -- Events are received synchronously on this connector and checked for the number of events of that ID.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
      Status : Two_Counter_Entry.Count_Status;
   begin
      -- Regardless of the global state, we call the increment. The global component enable/disable state as well as each individual id state is handled in the increment for efficiency.
      Self.Event_Array.Increment_Counter (Arg.Header.Id, Status);
      -- Only when status is an Event_Max_Limit, then we don't do anything. Otherwise, the event gets passed on
      case Status is
         -- When invalid or successful, we just pass through the event (don't need to know if enabled or disabled)
         when Success | Invalid_Id =>
            Self.Event_Forward_T_Send_If_Connected (Arg);
            -- If we hit a max, then the package takes care of the accounting and knows it was enabled.
         when Event_Max_Limit =>
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
   --    These are the commands for the event limiter component.
   -- Enable the event limiter for a specific event ID.
   overriding function Enable_Event_Limit (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Limiter_Enums;
      Status : Two_Counter_Entry.Enable_State_Status;
      Ret : Command_Execution_Status.E := Success;
   begin
      Self.Event_Array.Set_Enable_State (Arg.Event_To_Update.Id, Event_State_Type.Enabled, Status);
      case Status is
         when Invalid_Id =>
            Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Enable_Invalid_Id (Self.Sys_Time_T_Get, Arg));
            return Failure;
         when Success =>
            Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Enabled (Self.Sys_Time_T_Get, Arg));
      end case;

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Enable_Event_Limit;

   -- Disable the event limiter for a specific event ID.
   overriding function Disable_Event_Limit (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Limiter_Enums;
      Status : Two_Counter_Entry.Enable_State_Status;
      Ret : Command_Execution_Status.E := Success;
   begin
      Self.Event_Array.Set_Enable_State (Arg.Event_To_Update.Id, Event_State_Type.Disabled, Status);
      case Status is
         when Invalid_Id =>
            Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Disable_Invalid_Id (Self.Sys_Time_T_Get, Arg));
            return Failure;
         when Success =>
            Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Disabled (Self.Sys_Time_T_Get, Arg));
      end case;

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Disable_Event_Limit;

   -- Enable the event limiter for a specific range of event ID.
   overriding function Enable_Event_Limit_Range (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Limiter_Enums;
      Status : Two_Counter_Entry.Enable_State_Status;
      Ret : Command_Execution_Status.E := Success;
      Id_Stop : Event_Types.Event_Id;
      Id_Start : constant Event_Types.Event_Id := Self.Event_Array.Get_Event_Start_Stop_Range (Id_Stop);
   begin
      -- Make sure the range of ids will not be invalid before attempting to change all in the range
      if Id_Start <= Arg.Start_Event_Id.Id and then Id_Stop >= Arg.Stop_Event_Id.Id and then Arg.Start_Event_Id.Id <= Arg.Stop_Event_Id.Id then
         for Event_Id_State_Change in Arg.Start_Event_Id.Id .. Arg.Stop_Event_Id.Id loop
            Self.Event_Array.Set_Enable_State (Event_Id_State_Change, Event_State_Type.Enabled, Status);
            case Status is
               when Invalid_Id =>
                  pragma Assert (False, "Found Invalid_Id for the Event Limiter when commanding enable range of events, which should have been caught in an earlier statement");
               when Success =>
                  null; -- expected so continue to loop and do nothing in this case
            end case;
         end loop;
      else -- The provided event id's were out of range here.
         Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Range_Enabled_Invalid_Id (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;
      Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Range_Enabled (Self.Sys_Time_T_Get, Arg));

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Enable_Event_Limit_Range;

   -- Disable the event limiter for a specific range of event ID.
   overriding function Disable_Event_Limit_Range (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_Limiter_Enums;
      Status : Two_Counter_Entry.Enable_State_Status;
      Ret : Command_Execution_Status.E := Success;
      Id_Stop : Event_Types.Event_Id;
      Id_Start : constant Event_Types.Event_Id := Self.Event_Array.Get_Event_Start_Stop_Range (Id_Stop);
   begin
      -- Make sure the range of ids will not be invalid before attempting to change all in the range
      if Id_Start <= Arg.Start_Event_Id.Id and then Id_Stop >= Arg.Stop_Event_Id.Id and then Arg.Start_Event_Id.Id <= Arg.Stop_Event_Id.Id then
         for Event_Id_State_Change in Arg.Start_Event_Id.Id .. Arg.Stop_Event_Id.Id loop
            Self.Event_Array.Set_Enable_State (Event_Id_State_Change, Event_State_Type.Disabled, Status);
            case Status is
               when Invalid_Id =>
                  pragma Assert (False, "Found Invalid_Id for the Event Limiter when commanding disable range of events, which should have been caught in an earlier statement");
               when Success =>
                  null; -- expected so continue to loop and do nothing in this case
            end case;
         end loop;
      else -- The provided event id's were out of range here.
         Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Range_Disabled_Invalid_Id (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;
      Self.Event_T_Send_If_Connected (Self.Events.Event_Limit_Range_Disabled (Self.Sys_Time_T_Get, Arg));

      -- Now check if ground wants to dump the state packet
      case Arg.Issue_State_Packet is
         when Issue_Packet_Type.Issue =>
            Ret := Self.Dump_Event_States;
         when Issue_Packet_Type.No_Issue =>
            null; -- Don't send a packet so nothing to do
      end case;

      return Ret;
   end Disable_Event_Limit_Range;

   -- Enable the event limiters for all event IDs.
   overriding function Enable_Event_Limiting (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_State_Type;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- All we need to do here is enable the component level variable
      Self.Event_Array.Set_Master_Enable_State (Enabled);
      Self.Event_T_Send_If_Connected (Self.Events.Event_Limiting_Enabled (Timestamp));
      -- Update the data product as well
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Component_Limiting_Enabled_Status (Timestamp, (Component_Enable_State => Self.Event_Array.Get_Master_Enable_State)));

      return Success;
   end Enable_Event_Limiting;

   -- Disable the event limiters for all event IDs.
   overriding function Disable_Event_Limiting (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Event_State_Type;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      --Disable the component for event limiting
      Self.Event_Array.Set_Master_Enable_State (Disabled);
      Self.Event_T_Send_If_Connected (Self.Events.Event_Limiting_Disabled (Timestamp));
      -- Update the data product as well
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Component_Limiting_Enabled_Status (Timestamp, (Component_Enable_State => Self.Event_Array.Get_Master_Enable_State)));

      return Success;
   end Disable_Event_Limiting;

   -- Change the persistence of the event limiter for all events that are limited. Value must be between 0 and 7.
   overriding function Set_Event_Limit_Persistence (Self : in out Instance; Arg : in Event_Limiter_Persistence_Type.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Set then get it back from the package and send it in the event as a verification that it was set
      Self.Event_Array.Set_Persistence (Arg.Persistence);
      Self.Event_T_Send_If_Connected (Self.Events.Set_New_Persistence (Self.Sys_Time_T_Get, (Persistence => Self.Event_Array.Get_Persistence)));
      return Success;
   end Set_Event_Limit_Persistence;

   -- Dump a packet for the state of all events on if they are limited or not.
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
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

end Component.Event_Limiter.Implementation;
