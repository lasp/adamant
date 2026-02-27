--------------------------------------------------------------------------------
-- Fault_Correction Component Implementation Body
--------------------------------------------------------------------------------

with Safe_Deallocator;
with Packed_Fault_Response_Statuses;
with Data_Product_Types;

package body Component.Fault_Correction.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing an access to a list of fault response configuration records.
   --
   -- Init Parameters:
   -- Fault_Response_Configurations : Fault_Correction_Types.Fault_Response_Config_List - An access to a list of fault response configurations.
   --
   overriding procedure Init (Self : in out Instance; Fault_Response_Configurations : in Fault_Correction_Types.Fault_Response_Config_List) is
      use Fault_Correction_Enums.Startup_Status_Type;
      Table_Index : Fault_Response_Table_Index := Fault_Response_Table_Index'First;
   begin
      -- The binary tree package needs to be allocated with a positive. This component is completely
      -- useless without a single fault response.
      pragma Assert (Fault_Response_Configurations'Length > 0, "Empty configuration list is not allowed.");

      -- Make sure we will be able to fit all the statuses for each fault response entry into a single
      -- data product. The design of this component currently requires this assumption be met in order
      -- to produce that data product. There are four statuses included per byte in the data product.
      pragma Assert (((Fault_Response_Configurations'Length + 3) / 4) <= Data_Product_Types.Data_Product_Buffer_Type'Length,
         "Too many fault entries! Cannot fit statuses for all fault responses in a single data product.");

      -- Allocate space for the response lookup tree and table on the heap:
      Self.Fault_Response_Lookup.Init (Fault_Response_Configurations'Length);
      Self.Fault_Response_Table := new Fault_Response_Table_Type (
         Fault_Response_Table_Index'First ..
         Fault_Response_Table_Index'First + Fault_Response_Configurations'Length - 1
      );

      -- Store elements in the lookup and table:
      for Config of Fault_Response_Configurations loop
         declare
            -- Form lookup entry:
            Ignore_1 : Fault_Response_Lookup_Entry;
            Ignore_2 : Natural;
            Ret : Boolean;
            Lookup_Entry : constant Fault_Response_Lookup_Entry := (
               Id => Config.Id,
               Table_Index => Table_Index
            );
            -- Form table entry:
            Response_Entry : Fault_Response_Table_Entry := (
               Latching => Config.Latching,
               Status => Fault_Correction_Enums.Status_Type.Nominal,
               Command_Response => Config.Command_Response
            );
         begin
            -- Set the status to disabled if we need to.
            if Config.Startup_State = Disabled then
               Response_Entry.Status := Fault_Correction_Enums.Status_Type.Disabled;
            end if;

            -- Make sure the Fault ID is not already stored in lookup. We require a unique set.
            Ret := Self.Fault_Response_Lookup.Search (Lookup_Entry, Ignore_1, Ignore_2);
            pragma Assert (not Ret, "Duplicate Fault ID '" & Fault_Id'Image (Lookup_Entry.Id) & "' not allowed in fault response table!");
            -- Add entry to the lookup:
            Ret := Self.Fault_Response_Lookup.Add (Lookup_Entry);
            pragma Assert (Ret, "Binary tree too small to hold entry. This should never happen unless there is a bug.");

            -- OK, now add response entry to table:
            Self.Fault_Response_Table.all (Table_Index) := Response_Entry;
            Table_Index := @ + 1;
         end;
      end loop;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Fault_Response_Table_Type,
         Name => Fault_Response_Table_Type_Access
      );
   begin
      Free_If_Testing (Self.Fault_Response_Table);
      Self.Fault_Response_Lookup.Destroy;
   end Final;

   ---------------------------------------
   -- Helper subprograms:
   ---------------------------------------
   function Get_Fault_Response_Table_Index (Self : in out Instance; Id : in Fault_Types.Fault_Id; Index : out Fault_Response_Table_Index) return Boolean with
      -- The output Index should always be in range of the fault response table.
      Post => (Index >= Self.Fault_Response_Table.all'First and then Index <= Self.Fault_Response_Table.all'Last)
   is
      -- Form lookup entry:
      Lookup_Entry : Fault_Response_Lookup_Entry;
      Ignore : Natural;
      Lookup_Query : constant Fault_Response_Lookup_Entry := (
         Id => Id,
         Table_Index => Fault_Response_Table_Index'First
      );
      -- Search for Fault ID in table.
      Ret : constant Boolean := Self.Fault_Response_Lookup.Search (
         Element => Lookup_Query,
         Element_Found => Lookup_Entry,
         Element_Index => Ignore
      );
   begin
      -- Make sure entry found otherwise send event and return False;
      case Ret is
         when True =>
            Index := Lookup_Entry.Table_Index;
            return True;
         when False =>
            Index := Fault_Response_Table_Index'First;
            Self.Event_T_Send_If_Connected (Self.Events.Unrecognized_Fault_Id (Self.Sys_Time_T_Get, (Id => Id)));
            return False;
      end case;
   end Get_Fault_Response_Table_Index;

   procedure Send_Fault_Response_Statuses_Data_Product (Self : in out Instance; The_Time : in Sys_Time.T) is
   begin
      if Self.Is_Data_Product_T_Send_Connected then
         declare
            -- Create the dummy data product.
            To_Send : Data_Product.T := Self.Data_Products.Fault_Response_Statuses (The_Time, (Value => 0));
            -- Create index to index into data product buffer.
            Product_Buffer_Index : Data_Product_Types.Data_Product_Buffer_Length_Type :=
               Data_Product_Types.Data_Product_Buffer_Index_Type'First;
            -- Create a status record which will be used to fill the data product and overlay it over
            -- a single byte that can be used to fill the data product byte array.
            pragma Warnings (Off, "condition is always False");
            pragma Compile_Time_Error (Packed_Fault_Response_Statuses.Size_In_Bytes /= 1,
               "The code assumes that the Packed_Fault_Response_Statuses packed record is exactly 1 byte in size.");
            pragma Warnings (On, "condition is always False");
            Status_Byte : Basic_Types.Byte := 0;
            Status_Record : Packed_Fault_Response_Statuses.T with Import, Convention => Ada, Address => Status_Byte'Address;
         begin
            -- Fill the data product with the actual status data:
            for Index in Self.Fault_Response_Table.all'Range loop
               declare
                  -- Extract the status for this entry:
                  Status : Fault_Correction_Enums.Status_Type.E renames Self.Fault_Response_Table.all (Index).Status;

                  -- Determine where in the data product record to save the status:
                  type Record_Slot_Type is new Natural range 0 .. 3;
                  Record_Slot : constant Record_Slot_Type := Record_Slot_Type (Natural (Index - Self.Fault_Response_Table.all'First) mod 4);
               begin
                  -- Save the status into the record:
                  case Record_Slot is
                     when 0 =>
                        Status_Record.Status_0 := Status;
                     when 1 =>
                        Status_Record.Status_1 := Status;
                     when 2 =>
                        Status_Record.Status_2 := Status;
                     when 3 =>
                        Status_Record.Status_3 := Status;
                  end case;

                  -- Save the record into the data product if we have filled up the
                  -- status byte or are at the end of the loop.
                  if Record_Slot = 3 or else Index = Self.Fault_Response_Table.all'Last then
                     -- Note: We won't overflow this based on assertions in Init.
                     To_Send.Buffer (Product_Buffer_Index) := Status_Byte;
                     pragma Annotate (GNATSAS, False_Positive, "array index check", "Product_Buffer_Index bounds guaranteed by assertions in the Init");
                     if Product_Buffer_Index < Data_Product_Types.Data_Product_Buffer_Length_Type'Last then
                        Product_Buffer_Index := @ + 1;
                     end if;
                     Status_Byte := 0;
                  end if;
               end;
            end loop;

            -- Set the length based on the number of entries. Always round up to nearest byte:
            To_Send.Header.Buffer_Length := Product_Buffer_Index - Data_Product_Types.Data_Product_Buffer_Index_Type'First;

            -- Send out the data product:
            Self.Data_Product_T_Send (To_Send);
         end;
      end if;
   end Send_Fault_Response_Statuses_Data_Product;

   ---------------------------------------
   -- Setup subprogram
   ---------------------------------------

   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Fault_Counter (The_Time, (Value => Self.Fault_Counter)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Fault_Id_Received (The_Time, (Id => Fault_Id'First)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_Of_Last_Fault_Received (The_Time, (0, 0)));
      Self.Send_Fault_Response_Statuses_Data_Product (The_Time);
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- Faults are received asynchronously on this connector.
   overriding procedure Fault_T_Recv_Async (Self : in out Instance; Arg : in Fault.T) is
      -- See if we have an entry for the fault ID.
      Index : Fault_Response_Table_Index;
      Entry_Found : constant Boolean := Self.Get_Fault_Response_Table_Index (Id => Arg.Header.Id, Index => Index);
   begin
      -- If we have no entry then give up. An event has already been thrown. There is nothing else
      -- we can do.
      case Entry_Found is
         when True => null; -- Keep going.
         when False => return;
      end case;

      declare
         use Fault_Correction_Enums.Status_Type;
         use Fault_Correction_Enums.Latching_Type;
         Response_Sent : Boolean := False;
         Status_Changed : Boolean := False;
         -- Grab the table entry:
         Table_Entry : Fault_Response_Table_Entry renames Self.Fault_Response_Table.all (Index);
      begin
         -- Check the table entry status:
         case Table_Entry.Status is
            when Nominal =>
               -- Transition status.
               case Table_Entry.Latching is
                  when Non_Latching =>
                     Table_Entry.Status := Fault_Detected;
                  when Latching =>
                     Table_Entry.Status := Fault_Latched;
               end case;
               Status_Changed := True;

               -- Send the fault response:
               Self.Command_T_Send_If_Connected (Table_Entry.Command_Response);
               Response_Sent := True;
            when Fault_Detected =>
               -- Send the fault response:
               Self.Command_T_Send_If_Connected (Table_Entry.Command_Response);
               Response_Sent := True;
            when Fault_Latched => null; -- Nothing to do
            when Disabled => null; -- Nothing to do
         end case;

         -- Send the fault response could be time critical, so we do it as soon as possible. Now we send info events and
         -- data products as necessary.
         declare
            The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
            Param_Buffer : Fault_Types.Parameter_Buffer_Type := [others => 0];
         begin
            -- Copy over info from fault into statically sized fault type for serialization into info event:
            Param_Buffer (Param_Buffer'First .. Param_Buffer'First + Arg.Header.Param_Buffer_Length - 1) :=
               Arg.Param_Buffer (Arg.Param_Buffer'First .. Arg.Param_Buffer'First + Arg.Header.Param_Buffer_Length - 1);
            Self.Event_T_Send_If_Connected (Self.Events.Fault_Received (The_Time, (Header => Arg.Header, Param_Buffer => Param_Buffer)));
            -- Send response event if necessary:
            if Response_Sent then
               Self.Event_T_Send_If_Connected (Self.Events.Fault_Response_Sent (The_Time, Table_Entry.Command_Response.Header));
            end if;

            -- Send data products if necessary:
            Self.Fault_Counter := @ + 1;
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Fault_Counter (The_Time, (Value => Self.Fault_Counter)));
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Fault_Id_Received (The_Time, (Id => Arg.Header.Id)));
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_Of_Last_Fault_Received (The_Time, Arg.Header.Time));
            if Status_Changed then
               Self.Send_Fault_Response_Statuses_Data_Product (The_Time);
            end if;
         end;
      end;
   end Fault_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Command_Dropped (
         Self.Sys_Time_T_Get, Arg.Header
      ));
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Fault_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Fault_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Fault.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Fault_Dropped (
         Self.Sys_Time_T_Get, Arg.Header
      ));
   end Fault_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Fault Correction component.
   -- Enable a fault response for the provided ID. This will only succeed if another response with the same Fault ID is not already enabled.
   overriding function Enable_Fault_Response (Self : in out Instance; Arg : in Packed_Fault_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- See if we have an entry for the provided fault ID.
      Index : Fault_Response_Table_Index;
      Entry_Found : constant Boolean := Self.Get_Fault_Response_Table_Index (Id => Arg.Id, Index => Index);
   begin
      -- If we have no entry fail the command. An event has already been sent.
      case Entry_Found is
         when True => null; -- Keep going.
         when False => return Failure;
      end case;

      declare
         use Fault_Correction_Enums.Status_Type;
         -- Grab the table entry:
         Table_Entry : Fault_Response_Table_Entry renames Self.Fault_Response_Table.all (Index);
      begin
         -- Enable the response if disabled. If the status is in any other state (i.e. Fault)
         -- we are just going to leave it.
         case Table_Entry.Status is
            when Disabled =>
               Table_Entry.Status := Nominal;
            when Nominal | Fault_Latched | Fault_Detected =>
               null; -- Nothing to do.
         end case;
      end;

      declare
         The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      begin
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Fault_Response_Enabled (The_Time, Arg));

         -- Send data product:
         Self.Send_Fault_Response_Statuses_Data_Product (The_Time);
      end;

      return Success;
   end Enable_Fault_Response;

   -- Disable a fault response for the provided ID.
   overriding function Disable_Fault_Response (Self : in out Instance; Arg : in Packed_Fault_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- See if we have an entry for the provided fault ID.
      Index : Fault_Response_Table_Index;
      Entry_Found : constant Boolean := Self.Get_Fault_Response_Table_Index (Id => Arg.Id, Index => Index);
   begin
      -- If we have no entry fail the command. An event has already been sent.
      case Entry_Found is
         when True => null; -- Keep going.
         when False => return Failure;
      end case;

      declare
         use Fault_Correction_Enums.Status_Type;
         -- Grab the table entry:
         Table_Entry : Fault_Response_Table_Entry renames Self.Fault_Response_Table.all (Index);
      begin
         -- Disable the entry, it does not matter what state it is in.
         Table_Entry.Status := Disabled;
      end;

      declare
         The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      begin
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Fault_Response_Disabled (The_Time, Arg));

         -- Send data product:
         Self.Send_Fault_Response_Statuses_Data_Product (The_Time);
      end;

      return Success;
   end Disable_Fault_Response;

   -- Resets a fault response to the Enabled state of the provided ID. If the fault is latched, it unlatches the fault.
   overriding function Clear_Fault_Response (Self : in out Instance; Arg : in Packed_Fault_Id.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- See if we have an entry for the provided fault ID.
      Index : Fault_Response_Table_Index;
      Entry_Found : constant Boolean := Self.Get_Fault_Response_Table_Index (Id => Arg.Id, Index => Index);
   begin
      -- If we have no entry fail the command. An event has already been sent.
      case Entry_Found is
         when True => null; -- Keep going.
         when False => return Failure;
      end case;

      declare
         use Fault_Correction_Enums.Status_Type;
         -- Grab the table entry:
         Table_Entry : Fault_Response_Table_Entry renames Self.Fault_Response_Table.all (Index);
      begin
         -- Clear the fault response if we are in a fault state.
         case Table_Entry.Status is
            when Fault_Latched | Fault_Detected =>
               Table_Entry.Status := Nominal;
            when Nominal | Disabled =>
               null; -- Nothing to do.
         end case;
      end;

      declare
         The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      begin
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Fault_Response_Cleared (The_Time, Arg));

         -- Send data product:
         Self.Send_Fault_Response_Statuses_Data_Product (The_Time);
      end;

      return Success;
   end Clear_Fault_Response;

   -- Resets all fault responses to the Enabled state. Unlatches all latched fault responses.
   overriding function Clear_All_Fault_Responses (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      for Index in Self.Fault_Response_Table.all'Range loop
         declare
            use Fault_Correction_Enums.Status_Type;
            -- Grab the table entry:
            Table_Entry : Fault_Response_Table_Entry renames Self.Fault_Response_Table.all (Index);
         begin
            -- Clear the fault response if we are in a fault state.
            case Table_Entry.Status is
               when Fault_Latched | Fault_Detected =>
                  Table_Entry.Status := Nominal;
               when Nominal | Disabled =>
                  null; -- Nothing to do.
            end case;
         end;
      end loop;

      declare
         The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      begin
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.All_Fault_Responses_Cleared (The_Time));

         -- Send data product:
         Self.Send_Fault_Response_Statuses_Data_Product (The_Time);
      end;

      return Success;
   end Clear_All_Fault_Responses;

   -- This command resets the values of all the component's data product to the values at initialization, except for the Fault_Response_Statuses data product which can be reset by the Clear_All_Fault_Responses command.
   overriding function Reset_Data_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Reset the data:
      Self.Fault_Counter := 0;

      -- Send the data products:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Fault_Counter (The_Time, (Value => Self.Fault_Counter)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Last_Fault_Id_Received (The_Time, (Id => Fault_Id'First)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_Of_Last_Fault_Received (The_Time, (0, 0)));

      -- Send informational event saying that we got the command.
      Self.Event_T_Send_If_Connected (Self.Events.Data_Products_Reset (The_Time));
      return Success;
   end Reset_Data_Products;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Fault_Correction.Implementation;
