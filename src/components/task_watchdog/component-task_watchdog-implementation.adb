--------------------------------------------------------------------------------
-- Task_Watchdog Component Implementation Body
--------------------------------------------------------------------------------

with Packed_Missed_Pet_Limit;
with Packed_Connector_Index;
with Packed_Watchdog_Pet_States;

package body Component.Task_Watchdog.Implementation is

   --------------------------------------------------
   -- Protected object to handle internal counters for the watchdog petters:
   --------------------------------------------------
   protected body Protected_Watchdog_Entries is
      procedure Init (Task_Watchdog_Entry_Init_List : in Task_Watchdog_Init_List) is
      begin
         Protected_Watchdog_Entries_Package.Init (Task_Watchdog_Entry_Init_List);
      end Init;
      -- Procedures to set data in the watchdog list
      procedure Reset_Pet_Count (Index : in Connector_Types.Connector_Index_Type) is
      begin
         Protected_Watchdog_Entries_Package.Reset_Pet_Count (Index);
      end Reset_Pet_Count;

      procedure Set_Pet_Limit (Index : in Connector_Types.Connector_Index_Type; New_Limit : in Missed_Pet_Limit_Type) is
      begin
         Protected_Watchdog_Entries_Package.Set_Pet_Limit (Index, New_Limit);
      end Set_Pet_Limit;

      procedure Set_Pet_Action (Index : in Connector_Types.Connector_Index_Type; New_Action : in Watchdog_Action_State.E) is
      begin
         Protected_Watchdog_Entries_Package.Set_Pet_Action (Index, New_Action);
      end Set_Pet_Action;

      -- Function to get the respective item from our data structure when given an index.
      function Check_Watchdog_Pets (Index : in Connector_Types.Connector_Index_Type; Is_Critical : out Boolean) return Check_Status is
      begin
         return Protected_Watchdog_Entries_Package.Check_Watchdog_Pets (Index, Is_Critical);
      end Check_Watchdog_Pets;

      function Get_Pet_Criticality (Index : in Connector_Types.Connector_Index_Type) return Boolean is
      begin
         return Protected_Watchdog_Entries_Package.Get_Pet_Criticality (Index);
      end Get_Pet_Criticality;

      function Get_Pet_Action (Index : in Connector_Types.Connector_Index_Type) return Watchdog_Action_State.E is
      begin
         return Protected_Watchdog_Entries_Package.Get_Pet_Action (Index);
      end Get_Pet_Action;

      function Get_Pet_Action_Id (Index : in Connector_Types.Connector_Index_Type) return Fault_Types.Fault_Id is
      begin
         return Protected_Watchdog_Entries_Package.Get_Pet_Action_Id (Index);
      end Get_Pet_Action_Id;

      function Get_Pet_Has_Fault (Index : in Connector_Types.Connector_Index_Type) return Boolean is
      begin
         return Protected_Watchdog_Entries_Package.Get_Pet_Has_Fault (Index);
      end Get_Pet_Has_Fault;

      function Get_Pet_Count_Limit (Index : in Connector_Types.Connector_Index_Type) return Missed_Pet_Limit_Type is
      begin
         return Protected_Watchdog_Entries_Package.Get_Pet_Count_Limit (Index);
      end Get_Pet_Count_Limit;

   end Protected_Watchdog_Entries;

   --------------------------------------------------
   -- Helper functions for creating faults and data products:
   --------------------------------------------------
   -- Fault creation and send helper function
   procedure Send_Fault (Self : in out Instance; Index : in Connector_Types.Connector_Index_Type; Timestamp : in Sys_Time.T) is
      New_Fault : Fault.T;
   begin
      if Self.Is_Fault_T_Send_Connected then
         New_Fault.Header.Id := Self.Task_Watchdog_Entries.Get_Pet_Action_Id (Index);
         New_Fault.Header.Time := Timestamp;
         New_Fault.Header.Param_Buffer_Length := Packed_Connector_Index.Size_In_Bytes;

         New_Fault.Param_Buffer (0 .. Packed_Connector_Index.Size_In_Bytes - 1) := Packed_Connector_Index.Serialization.To_Byte_Array ((Index => Index));
         -- Send out the fault:
         Self.Fault_T_Send (New_Fault);
      end if;
   end Send_Fault;

   -- Limit data product helper function
   procedure Send_Limit_Dp (Self : in out Instance; Index : in Connector_Types.Connector_Index_Type; Timestamp : in Sys_Time.T; Tick_Limit : in Missed_Pet_Limit_Type) is
      use Data_Product_Types;
      Dp : Data_Product.T;
   begin
      -- Make sure we are connected
      if Self.Is_Data_Product_T_Send_Connected then
         -- Fill in the data product information. Add one to the index because the first two data products are defined so the first index is at Get_Id_Base + 2.
         -- Since Index starts at 1 and not 0, we just add 1 to the index to get our id.
         Dp.Header.Id := (Self.Data_Products.Get_Id_Base + Data_Product_Types.Data_Product_Id (Index) + 1);
         Dp.Header.Time := Timestamp;
         Dp.Header.Buffer_Length := Packed_Missed_Pet_Limit.Size_In_Bytes;

         Dp.Buffer (0 .. Packed_Missed_Pet_Limit.Size_In_Bytes - 1) := Packed_Missed_Pet_Limit.Serialization.To_Byte_Array ((Limit => Tick_Limit));
         -- Send out the data product:
         Self.Data_Product_T_Send (Dp);
      end if;
   end Send_Limit_Dp;

   -- Enumerated state data product helper function
   procedure Send_Action_Dp (Self : in out Instance; The_Time : in Sys_Time.T) is
   begin
      if Self.Is_Data_Product_T_Send_Connected then
         declare
            -- Create the dummy data product.
            To_Send : Data_Product.T := Self.Data_Products.Pet_Connector_Action_States (The_Time, (Value => 0));
            -- Create index to index into data product buffer.
            Product_Buffer_Index : Data_Product_Types.Data_Product_Buffer_Length_Type := Data_Product_Types.Data_Product_Buffer_Index_Type'First;
            -- Create a status record which will be used to fill the data product and overlay it over
            -- a single byte that can be used to fill the data product byte array.
            State_Byte : Basic_Types.Byte := 0;
            State_Record : Packed_Watchdog_Pet_States.T with
               Import,
               Convention => Ada,
               Address => State_Byte'Address;
         begin
            -- Fill the data product with the actual status data:
            for Index in Pet_T_Recv_Sync_Index'First .. Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1 loop
               declare
                  -- Extract the status for this entry:
                  State : Watchdog_Action_State.E renames Self.Task_Watchdog_Entries.Get_Pet_Action (Index);

                  -- Determine where in the data product record to save the status:
                  type Record_Slot_Type is new Natural range 0 .. 3;
                  Record_Slot : constant Record_Slot_Type := Record_Slot_Type (Natural (Index - Pet_T_Recv_Sync_Index'First) mod 4);
               begin
                  -- Save the status into the record:
                  case Record_Slot is
                     when 0 =>
                        State_Record.State_0 := State;
                     when 1 =>
                        State_Record.State_1 := State;
                     when 2 =>
                        State_Record.State_2 := State;
                     when 3 =>
                        State_Record.State_3 := State;
                  end case;

                  -- Save the record into the data product if we have filled up the
                  -- status byte or are at the end of the loop.
                  if Record_Slot = 3 or else Index = (Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1) then
                     -- Note: We won't overflow this based on assertions in Init.
                     To_Send.Buffer (Product_Buffer_Index) := State_Byte;
                     pragma Annotate (GNATSAS, False_Positive, "array index check", "Product_Buffer_Index bounds guaranteed by assertions in the Init");
                     if Product_Buffer_Index < Data_Product_Types.Data_Product_Buffer_Length_Type'Last then
                        Product_Buffer_Index := @ + 1;
                     end if;
                     State_Byte := 0;
                  end if;
               end;
            end loop;

            -- Set the length based on the number of entries. Always round up to nearest byte:
            To_Send.Header.Buffer_Length := Product_Buffer_Index - Data_Product_Types.Data_Product_Buffer_Index_Type'First;

            -- Send out the data product:
            Self.Data_Product_T_Send (To_Send);
         end;
      end if;
   end Send_Action_Dp;

   --------------------------------------------------
   -- Subprogram for implementation Set_Up method:
   --------------------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Pet_Count_Limit : Missed_Pet_Limit_Type;
   begin
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Watchdog_Component_Petter_State (Timestamp, (State => Self.Task_Watchdog_Component_State)));
      Self.Send_Action_Dp (Timestamp);
      -- Send out data products that are constructed from the init list. This means we have to find each one in the list
      for Idx in Pet_T_Recv_Sync_Index'First .. Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1 loop
         Pet_Count_Limit := Self.Task_Watchdog_Entries.Get_Pet_Count_Limit (Idx);
         Self.Send_Limit_Dp (Idx, Timestamp, Pet_Count_Limit);
      end loop;
   end Set_Up;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Task_Watchdog_Entry_Init_List : Task_Watchdog_Types.Task_Watchdog_Init_List - The list of components that have a watchdog to pet that need to be tracked by the task watchdog.
   --
   overriding procedure Init (Self : in out Instance; Task_Watchdog_Entry_Init_List : in Task_Watchdog_Types.Task_Watchdog_Init_List) is
   begin
      -- Make sure our list is not empty and then pass it to the protected object
      pragma Assert (Task_Watchdog_Entry_Init_List'Length > 0, "Watchdog Init Entry list must have one petter to check.");
      Self.Task_Watchdog_Entries.Init (Task_Watchdog_Entry_Init_List);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The schedule invokee connector.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Watchdog_Enabled_State;
      Critical_Tasks_Running_Well : Boolean := True;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      if Self.Task_Watchdog_Component_State = Enabled then
         -- For each entry in our watch list see if the task has timed out.
         for Idx in Pet_T_Recv_Sync_Index'First .. Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1 loop
            declare
               Is_Critical : Boolean;
               Status : constant Watchdog_List.Check_Status := Self.Task_Watchdog_Entries.Check_Watchdog_Pets (Idx, Is_Critical);
            begin
               case Status is
                  -- When the check on the incoming pet is disabled, do nothing.
                  when Disable =>
                     null;
                  -- When we know that the component is still petting, do nothing.
                  when Petting =>
                     null;
                  -- When the failure is a warn failure, send an event.
                  when Warn_Failure =>
                     Self.Event_T_Send_If_Connected (Self.Events.Component_Exceeded_Pet_Limit (Timestamp, (Index => Idx)));
                  -- When the failure is a fault failure, send the fault and the event
                  when Fault_Failure =>
                     Self.Send_Fault (Idx, Timestamp);
                     Self.Event_T_Send_If_Connected (Self.Events.Component_Exceeded_Pet_Limit (Timestamp, (Index => Idx)));
                  -- When it's a repeated failure, all we need to know is if it's critical which gets checked every time
                  when Repeat_Failure =>
                     null;
               end case;

               -- If critical, then set the flag not to pet the hardware watchdog and send an event.
               -- The critical flag should only be set to true if the task is critical and in an error state
               if Is_Critical then
                  Critical_Tasks_Running_Well := False;
                  Self.Event_T_Send_If_Connected (Self.Events.Critical_Task_Not_Petting (Timestamp, (Index => Idx)));
               end if;
            end;
         end loop;
      end if;

      -- If all the critical tasks are running properly then service the downstream watchdog:
      if Critical_Tasks_Running_Well then
         Self.Pet_T_Send_If_Connected ((Count => Arg.Count));
      end if;
   end Tick_T_Recv_Sync;

   -- The arrayed pet receive connector. Upstream components call this connector to let the Task Watchdog know they are running OK.
   overriding procedure Pet_T_Recv_Sync (Self : in out Instance; Index : in Pet_T_Recv_Sync_Index; Arg : in Pet.T) is
   begin
      -- OK we got a pet. The index should be in range or there is a bug in the autocode:
      pragma Assert (Index <= Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1, "Index is out of range for the task_watchdog pet connections.");

      -- Index into the watch list and reset count for this index to zero.
      Self.Task_Watchdog_Entries.Reset_Pet_Count (Index);
   end Pet_T_Recv_Sync;

   -- The command receive connector
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
   --    These are the commands for the Task Watchdog component.
   -- Command to enable the watchdog component to check all connected components for incoming pets.
   overriding function Enable_Watchdog_Pet_Checks (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Watchdog_Enabled_State;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Self.Task_Watchdog_Component_State := Enabled;
      Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Pet_Checks_Enabled (Timestamp));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Watchdog_Component_Petter_State (Timestamp, (State => Self.Task_Watchdog_Component_State)));
      return Success;
   end Enable_Watchdog_Pet_Checks;

   -- Command to disable the watchdog component to check all connected components for incoming pets.
   overriding function Disable_Watchdog_Pet_Checks (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Watchdog_Enabled_State;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Self.Task_Watchdog_Component_State := Disabled;
      Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Pet_Checks_Disabled (Timestamp));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Watchdog_Component_Petter_State (Timestamp, (State => Self.Task_Watchdog_Component_State)));
      return Success;
   end Disable_Watchdog_Pet_Checks;

   -- Set the limit value for the watchdog given an index and the new index value.
   overriding function Set_Watchdog_Limit (Self : in out Instance; Arg : in Watchdog_Limit_Cmd.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Connector_Index : constant Connector_Types.Connector_Index_Type := Arg.Index;
      Updated_Limit : Missed_Pet_Limit_Type;
   begin
      -- Check connector index to make sure its in range
      if Connector_Index <= (Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1) then
         Self.Task_Watchdog_Entries.Set_Pet_Limit (Connector_Index, Arg.New_Limit);
         -- Once set, send an event and update the data product
         Updated_Limit := Self.Task_Watchdog_Entries.Get_Pet_Count_Limit (Connector_Index);
         Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Limit_Set (Timestamp, (Index => Connector_Index, New_Limit => Updated_Limit)));
         Self.Send_Limit_Dp (Connector_Index, Timestamp, Updated_Limit);
         return Success;
      end if;

      -- Otherwise throw an error
      Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Limit_Change_Index_Out_Of_Range (Timestamp, (Index => Connector_Index)));
      return Failure;
   end Set_Watchdog_Limit;

   -- Sets the action of a petter given the index of that petter and the updated action. Note that actions cannot be promoted to fault if they were not provided a fault id.
   overriding function Set_Watchdog_Action (Self : in out Instance; Arg : in Watchdog_Action_Cmd.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Watchdog_Action_State;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Connector_Index : constant Connector_Types.Connector_Index_Type := Arg.Index;
      Updated_Action : Watchdog_Action_State.E;
   begin
      -- Check connector index to make sure its in range
      if Connector_Index > (Pet_T_Recv_Sync_Index'First + Self.Pet_T_Recv_Sync_Count - 1) then
         -- Out of range so send an error
         Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Action_Change_Index_Out_Of_Range (Timestamp, (Index => Connector_Index)));
         return Failure;
      end if;

      -- Now make sure that if we are going to the fault state, then we are able to do so.
      if Arg.New_Action = Error_Fault and then Self.Task_Watchdog_Entries.Get_Pet_Has_Fault (Connector_Index) = True then
         Self.Task_Watchdog_Entries.Set_Pet_Action (Connector_Index, Arg.New_Action);
      -- If the state is anything else but fault, we should just be able to set it.
      elsif Arg.New_Action /= Error_Fault then
         Self.Task_Watchdog_Entries.Set_Pet_Action (Connector_Index, Arg.New_Action);
      else
         -- If we tried to promote to a fault state but that is invalid for this entry, then fail out of the command.
         Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Action_Change_Invalid_Transition_To_Fault (Timestamp, (Index => Connector_Index)));
         return Failure;
      end if;
      -- Once set, send an event and update the data product
      Updated_Action := Self.Task_Watchdog_Entries.Get_Pet_Action (Connector_Index);
      Self.Event_T_Send_If_Connected (Self.Events.Watchdog_Action_Set (Timestamp, (Index => Connector_Index, New_Action => Updated_Action)));
      Self.Send_Action_Dp (Timestamp);
      return Success;
   end Set_Watchdog_Action;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

end Component.Task_Watchdog.Implementation;
