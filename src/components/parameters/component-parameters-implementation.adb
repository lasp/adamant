--------------------------------------------------------------------------------
-- Parameters Component Implementation Body
--------------------------------------------------------------------------------

with Parameter_Types;
with Parameter_Enums;
with Byte_Array_Pointer.Packed;
with Basic_Types;
with Memory_Region;
with Parameter_Table_Header;
with Serializer_Types;

package body Component.Parameters.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This init function provides the a list of parameter entries that describe the layout of the parameter table in memory. Calling this function also provides memory allocation for the parameter manager's internal parameter table. Preallocated memory can be provided via the "bytes" access type. Note the size of the preallocated memory MUST match the size of the parameter table exactly, as defined in the parameter_Entries parameter. If you would like to allocate the internal memory on the heap then "bytes" can be set to null.
   --
   -- Init Parameters:
   -- parameter_Entries : Parameters_Component_Types.Parameter_Entry_List_Access - A pointer to an autocoded list of parameter entries. This table tells the parameter manager how the parameters are laid out in memory, so that it knows how to construct parameter types to update downstream components.
   -- dump_Parameters_On_Change : Boolean - If set to True, the component will dump the current parameter values any time a command or memory region is received to alter one or more parameter values. If set to False, parameters will only be dumped when requested by command.
   --
   overriding procedure Init (Self : in out Instance; Parameter_Table_Entries : in Parameters_Component_Types.Parameter_Table_Entry_List_Access; Dump_Parameters_On_Change : in Boolean := False) is
      use Parameter_Types;
      use Parameters_Component_Types;
      Current_Byte : Natural := 0;
   begin
      -- Initialize internal variables:
      Self.Dump_Parameters_On_Change := Dump_Parameters_On_Change;

      -- Make sure the parameter entries is a valid access type. We can't do anything without this:
      pragma Assert (Parameter_Table_Entries /= null, "parameter_Table_Entries must not be null!");
      Self.Entries := Parameter_Table_Entries;

      --
      -- Let's do a few checks. This should be done by the autocoder, but there is no harm doing it
      -- at runtime as well.
      --
      for Idx in Self.Entries.all'Range loop
         declare
            Param_Entry : Parameters_Component_Types.Parameter_Table_Entry renames Self.Entries.all (Idx);
         begin
            -- Make sure all the IDs are unique.
            for Next_Entry_Idx in Natural range Idx + 1 .. Self.Entries.all'Last loop
               pragma Assert (Param_Entry.Id /= Self.Entries.all (Next_Entry_Idx).Id, "All parameter IDs must be unique. Duplicate ID '" & Parameter_Id'Image (Param_Entry.Id) & "' found.");
            end loop;

            -- Make sure all component IDs are within the index range of our arrayed parameter connector.
            pragma Assert (
               Param_Entry.Component_Id >= Self.Connector_Parameter_Update_T_Provide'First and then Param_Entry.Component_Id <= Self.Connector_Parameter_Update_T_Provide'Last,
               "Destination index for Parameter '" & Parameter_Id'Image (Param_Entry.Id) & " is out of range: " & Connector_Index_Type'Image (Param_Entry.Component_Id) & ". Must be between '" &
               Connector_Index_Type'Image (Self.Connector_Parameter_Update_T_Provide'First) & "' and '" & Connector_Index_Type'Image (Self.Connector_Parameter_Update_T_Provide'Last) & "'."
            );

            -- Make sure that the component ID is connected to a connector that is indeed connected.
            pragma Assert (Self.Is_Parameter_Update_T_Provide_Connected (Param_Entry.Component_Id),
               "Connector '" & Connector_Index_Type'Image (Param_Entry.Component_Id) & "' is unconnected, but has parameters that need to be serviced.");

            -- Make sure the parameter entries are layout in byte order, with no deadspace and no overlap, and are not too large.
            pragma Assert (Param_Entry.Start_Index = Current_Byte, "Unexpected byte layout in parameter table at ID '" & Parameter_Id'Image (Param_Entry.Id) & "'.");
            pragma Assert (Param_Entry.End_Index >= Param_Entry.Start_Index, "end_Index must be greated than start_Index at ID '" & Parameter_Id'Image (Param_Entry.Id) & "'.");
            pragma Assert (Param_Entry.End_Index - Param_Entry.Start_Index + 1 <= Parameter_Types.Parameter_Buffer_Length_Type'Last,
               "Parameter ID '" & Parameter_Id'Image (Param_Entry.Id) & "' is too large to fit in the parameter record.");
            Current_Byte := Param_Entry.End_Index + 1;
         end;
      end loop;

      --
      -- Save off a few constants. We use these all over the place, but it is code bloat to calculate them
      -- over and over again. So we will calculate them once here and store them in the component record:
      --
      -- Length of the parameter data in bytes:
      Self.Parameter_Table_Data_Length := Self.Entries.all (Self.Entries.all'Last).End_Index + 1;
      -- Length of the parameter table in total in bytes:
      Self.Parameter_Table_Length := Parameter_Table_Header.Size_In_Bytes + Self.Parameter_Table_Data_Length;
      pragma Assert (Self.Parameter_Table_Length + Crc_16.Crc_16_Type'Length <= Packet_Types.Packet_Buffer_Type'Length, "The parameter table must not be larger than the maximum size packet!");
   end Init;

   ---------------------------------------
   -- Helper functions:
   ---------------------------------------

   -- Helper function to fetch parameters from downstream components and store them in a buffer.
   -- The caller of this function provides the buffer, and the buffer must be large enough
   -- to hold the entire parameter table.
   function Fetch_Parameters (Self : in out Instance; Buffer : in out Basic_Types.Byte_Array) return Parameter_Enums.Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      To_Return : Parameter_Enums.Parameter_Update_Status.E := Success;
   begin
      -- The caller of this function should ensure that the buffer type is large enough to hold an
      -- entire parameter table:
      pragma Assert (Buffer'Length >= Self.Parameter_Table_Data_Length, "Buffer not large enough to hold parameter table data!");

      -- Go through all of our parameter table entries and fetch them one at a time from the connected
      -- components.
      for Param_Entry of Self.Entries.all loop
         declare
            use Parameter_Enums.Parameter_Operation_Type;
            Param_Update : Parameter_Update.T := (
               Operation => Fetch,
               Status => Success,
               Param => (Header => (Id => Param_Entry.Id, Buffer_Length => 0), Buffer => (others => 0)
            ));
            -- Calculate expected parameter length:
            Param_Length : constant Parameter_Types.Parameter_Buffer_Length_Type := Param_Entry.End_Index - Param_Entry.Start_Index + 1;
            -- Component index:
            Idx : constant Parameter_Update_T_Provide_Index := Parameter_Update_T_Provide_Index (Param_Entry.Component_Id);
         begin
            -- Send the parameter fetch request to the appropriate component:
            Self.Parameter_Update_T_Provide (Idx, Param_Update);

            -- Make sure the status is successful. If it is not, then produce an event, but still continue
            -- on to produce the packet:
            if Param_Update.Status = Success then
               -- Copy the data returned into the packet, unless there is a length mismatch. A length mismatch
               -- should never happen if the autocoded parameter entry table is error free.
               if Param_Length = Param_Update.Param.Header.Buffer_Length then
                  -- Copy parameter data into packet:
                  Buffer (Buffer'First + Param_Entry.Start_Index .. Buffer'First + Param_Entry.End_Index)
                     := Param_Update.Param.Buffer (Param_Update.Param.Buffer'First .. Param_Update.Param.Buffer'First + Param_Length - 1);
               else
                  Self.Event_T_Send_If_Connected (Self.Events.Parameter_Fetch_Length_Mismatch (Self.Sys_Time_T_Get, (
                     Header => Param_Update.Param.Header,
                     Expected_Length => Param_Length)
                  ));
                  To_Return := Length_Error;
               end if;
            else
               -- Send error event. This really should never happen if the autocoded parameter entry table
               -- is error free.
               Self.Event_T_Send_If_Connected (Self.Events.Parameter_Fetch_Failed (Self.Sys_Time_T_Get, (
                  Operation => Param_Update.Operation,
                  Status => Param_Update.Status,
                  Id => Param_Update.Param.Header.Id)
               ));
               To_Return := Param_Update.Status;
            end if;
         end;
      end loop;

      return To_Return;
   end Fetch_Parameters;

   -- Crc the parameter table bytes. The table bytes passed in MUST be the exact size as the parameter table:
   function Crc_Parameter_Table (Self : in Instance; Table_Bytes : in Basic_Types.Byte_Array) return Crc_16.Crc_16_Type is
   begin
      -- Thus function assumes that the provided data is the exact length of the parameter table. Length
      -- checks should be performed before calling this function:
      pragma Assert (Table_Bytes'Length = Self.Parameter_Table_Length);

      -- Some checks to make sure parameter table header constants make sense. This will fail if the
      -- header packed record and constants are malformed.
      pragma Assert (Parameter_Table_Header.Crc_Section_Length + Parameter_Table_Header.Version_Length = Parameter_Table_Header.Size_In_Bytes);

      -- Calculate the CRC over the version and data:
      return Crc_16.Compute_Crc_16 (Table_Bytes (
         Table_Bytes'First + Parameter_Table_Header.Crc_Section_Length ..
         Table_Bytes'First + Parameter_Table_Header.Crc_Section_Length + Parameter_Table_Header.Version_Length + Self.Parameter_Table_Data_Length - 1
      ));
   end Crc_Parameter_Table;

   -- Fetch parameters from all downstream components and form the parameter table data structure complete with the
   -- table header and computed CRC. The table is returned via the table_Bytes parameter, which MUST be the exact
   -- size as the parameter table.
   function Fetch_Parameter_Table (Self : in out Instance; Table_Bytes : in out Basic_Types.Byte_Array) return Parameter_Enums.Parameter_Update_Status.E is
      -- The return status:
      To_Return : Parameter_Enums.Parameter_Update_Status.E;
   begin
      -- Thus function assumes that the provided data is the exact length of the parameter table. Length
      -- checks should be performed before calling this function:
      pragma Assert (Table_Bytes'Length = Self.Parameter_Table_Length);

      -- Fill in the header data:
      Table_Bytes (Table_Bytes'First .. Table_Bytes'First + Parameter_Table_Header.Size_In_Bytes - 1)
         := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Self.Stored_Crc, Version => Self.Table_Version));

      -- Fetch the parameter values from the connected components and store them in the packet
      -- buffer.
      To_Return := Self.Fetch_Parameters (Table_Bytes (
         Table_Bytes'First + Parameter_Table_Header.Size_In_Bytes ..
         Table_Bytes'First + Self.Parameter_Table_Length - 1
      ));

      return To_Return;
   end Fetch_Parameter_Table;

   -- Helper function to construct and send a parameters packet filled with the parameter
   -- table data.
   function Send_Parameters_Packet (Self : in out Instance) return Parameter_Enums.Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      -- The return status:
      To_Return : Parameter_Enums.Parameter_Update_Status.E := Success;
   begin
      -- Only execute this logic if the packet connector is connected.
      if Self.Is_Packet_T_Send_Connected then
         declare
            -- Grab an empty packet that we can start filling in.
            Table_Bytes : Basic_Types.Byte_Array (0 .. Self.Parameter_Table_Length - 1);
         begin
            -- Send start event:
            Self.Event_T_Send_If_Connected (Self.Events.Dumping_Parameters (Self.Sys_Time_T_Get));

            -- Fetch the parameter table and store it in the temporary buffer:
            To_Return := Self.Fetch_Parameter_Table (Table_Bytes);

            declare
               use Serializer_Types;
               use Basic_Types;
               -- Calculate the crc of the table:
               Calculated_Crc : constant Crc_16.Crc_16_Type := Self.Crc_Parameter_Table (Table_Bytes);
               Pkt : Packet.T;
               -- Create the packet:
               Stat : constant Serialization_Status := Self.Packets.Active_Parameters (Self.Sys_Time_T_Get, Calculated_Crc & Table_Bytes, Pkt);
            begin
               -- Send the packet:
               pragma Assert (Stat = Success, "This should never fail since we checked at Init the parameter tables fits cleanly within a Packet.T type.");
               Self.Packet_T_Send (Pkt);
            end;

            -- Send end event:
            Self.Event_T_Send_If_Connected (Self.Events.Finished_Dumping_Parameters (Self.Sys_Time_T_Get));
         end;
      end if;

      return To_Return;
   end Send_Parameters_Packet;

   -- Helper function to copy the current values of the component's parameters into a memory region.
   -- It is assumed at this level that the memory region is large enough to hold the parameter table.
   function Copy_Parameter_Table_To_Region (Self : in out Instance; Region : in Memory_Region.T) return Parameters_Memory_Region_Release.T is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Basic_Types;
      Status : Parameter_Enums.Parameter_Update_Status.E;
      Status_To_Return : Parameter_Enums.Parameter_Table_Update_Status.E := Success;

      -- Create a buffer that overlays the memory region. We will use this buffer to store all the
      -- parameters from the components.
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Parameter_Table_Length - 1);
      Region_Byte_Array_Overlay : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Region.Address;
   begin
      -- We assume the region is large enough:
      pragma Assert (Region.Length >= Self.Parameter_Table_Length);

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Starting_Parameter_Table_Fetch (Self.Sys_Time_T_Get, Region));

      -- Fetch the parameter table and store it in the packet:
      Status := Self.Fetch_Parameter_Table (Region_Byte_Array_Overlay);
      if Status /= Success then
         Status_To_Return := Parameter_Error;
      end if;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Finished_Parameter_Table_Fetch (Self.Sys_Time_T_Get, (Region => Region, Status => Status_To_Return)));

      -- Return the memory pointer with the status for deallocation.
      return (Region => Region, Status => Status_To_Return);
   end Copy_Parameter_Table_To_Region;

   -- Helper function to stage a single parameter within a connected component.
   function Stage_Parameter (Self : in out Instance; Param_Entry : in Parameters_Component_Types.Parameter_Table_Entry; Value : in Parameter_Types.Parameter_Buffer_Type) return Parameter_Enums.Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      -- Calculate the parameter length:
      Param_Length : constant Parameter_Types.Parameter_Buffer_Length_Type := Param_Entry.End_Index - Param_Entry.Start_Index + 1;
      -- Create a parameter update record:
      Param_Update : Parameter_Update.T := (
         Operation => Stage,
         Status => Success,
         Param => (Header => (Id => Param_Entry.Id, Buffer_Length => Param_Length), Buffer => (others => 0)
      ));
      -- Component index:
      Idx : constant Parameter_Update_T_Provide_Index := Parameter_Update_T_Provide_Index (Param_Entry.Component_Id);
   begin
      -- Copy over value into record:
      Param_Update.Param.Buffer (Param_Update.Param.Buffer'First .. Param_Update.Param.Buffer'First + Param_Length - 1)
         := Value (Param_Update.Param.Buffer'First .. Param_Update.Param.Buffer'First + Param_Length - 1);

      -- Send the parameter fetch request to the appropriate component:
      Self.Parameter_Update_T_Provide (Idx, Param_Update);

      -- Make sure the status is successful. If it is not, then produce an event.
      if Param_Update.Status /= Success then
         Self.Event_T_Send_If_Connected (Self.Events.Parameter_Stage_Failed (Self.Sys_Time_T_Get, (
            Operation => Param_Update.Operation,
            Status => Param_Update.Status,
            Id => Param_Update.Param.Header.Id)
         ));
      end if;

      return Param_Update.Status;
   end Stage_Parameter;

   -- Helper function to send an update for a single components parameters:
   function Update_Parameters (Self : in out Instance; Component_Id : in Connector_Types.Connector_Index_Type) return Parameter_Enums.Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      -- Create a parameter update record:
      Param_Update : Parameter_Update.T := (
         Operation => Update,
         Status => Success,
         Param => (Header => (Id => 0, Buffer_Length => 0), Buffer => (others => 0)
      ));
      -- Component index:
      Idx : constant Parameter_Update_T_Provide_Index := Parameter_Update_T_Provide_Index (Component_Id);
   begin
      -- Send the parameter fetch request to the appropriate component:
      Self.Parameter_Update_T_Provide (Idx, Param_Update);

      -- Make sure the status is successful. If it is not, then produce an event.
      if Param_Update.Status /= Success then
         Self.Event_T_Send_If_Connected (Self.Events.Parameter_Update_Failed (Self.Sys_Time_T_Get, (
            Operation => Param_Update.Operation,
            Status => Param_Update.Status,
            Id => Param_Update.Param.Header.Id)
         ));
      end if;

      return Param_Update.Status;
   end Update_Parameters;

   -- Helper function to update all connected component's parameter tables from data within a provided memory region.
   function Update_Parameter_Table (Self : in out Instance; Region : in Memory_Region.T) return Parameters_Memory_Region_Release.T is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Table_Update_Status;
      Status_To_Return : Parameter_Enums.Parameter_Table_Update_Status.E := Success;
      Memory_Region_Ptr : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Packed.Unpack (Region);
      Parameter_Data_Ptr : constant Byte_Array_Pointer.Instance
         := Byte_Array_Pointer.Slice (Memory_Region_Ptr, Start_Index => Parameter_Table_Header.Size_In_Bytes);

      -- If the status returned is not success then set our status to return to a parameter error.
      procedure Set_Status (Stat : in Parameter_Enums.Parameter_Update_Status.E) is
      begin
         -- Check the return status. Even if it is bad, we still continue to try to stage
         -- the rest of the parameters.
         if Stat /= Success then
            Status_To_Return := Parameter_Error;
         end if;
      end Set_Status;
   begin
      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Starting_Parameter_Table_Update (Self.Sys_Time_T_Get, Region));

      -- Go through all our entries linearly, extracting data from the incoming memory region, and
      -- using the values stored there as values to stage our parameters.
      for Param_Entry of Self.Entries.all loop
         declare
            use Byte_Array_Pointer;
            -- Calculate the parameters length:
            Param_Length : constant Parameter_Types.Parameter_Buffer_Length_Type := Param_Entry.End_Index - Param_Entry.Start_Index + 1;
            -- Grab a slice of the memory region that contains our parameter's value:
            Ptr : constant Byte_Array_Pointer.Instance := Slice (
               Parameter_Data_Ptr,
               Start_Index => Param_Entry.Start_Index,
               End_Index => Param_Entry.End_Index
            );
            -- Create a temporary buffer to hold the parameter value:
            Value : Parameter_Types.Parameter_Buffer_Type := (others => 0);
         begin
            -- Copy the bytes from the pointer into a temporary buffer that is the maximum
            -- size of a parameter buffer:
            Value (Value'First .. Value'First + Param_Length - 1) := To_Byte_Array (Ptr);

            -- Stage the parameter:
            Set_Status (Self.Stage_Parameter (Param_Entry => Param_Entry, Value => Value));
         end;
      end loop;

      -- OK, now we need to update all the parameters.
      for Idx in Self.Connector_Parameter_Update_T_Provide'Range loop
         if Self.Is_Parameter_Update_T_Provide_Connected (Idx) then
            Set_Status (Self.Update_Parameters (Component_Id => Idx));
         end if;
      end loop;

      -- Send out a new parameter's packet if configured to do so:
      if Self.Dump_Parameters_On_Change then
         -- Send the packet:
         Set_Status (Self.Send_Parameters_Packet);
      end if;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Finished_Parameter_Table_Update (Self.Sys_Time_T_Get, (
         Region => Region,
         Status => Status_To_Return)
      ));

      -- Return the memory pointer with the status for deallocation.
      return (Region => Region, Status => Status_To_Return);
   end Update_Parameter_Table;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command recieve connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   overriding procedure Parameters_Memory_Region_T_Recv_Async (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      To_Return : Parameters_Memory_Region_Release.T := (Region => Arg.Region, Status => Success);
   begin
      -- First make sure that the memory region is of the expected size. If it is not then we will
      -- reject this request.
      if Arg.Region.Length /= Self.Parameter_Table_Length then
         Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (Parameters_Region => Arg, Expected_Length => Self.Parameter_Table_Length)));
         To_Return := (Region => Arg.Region, Status => Length_Error);
      else
         -- Perform the requested operation:
         case Arg.Operation is
            -- The memory region contains a fresh parameter table. We need to use this parameter table to
            -- update all the active parameters.
            when Set =>
               -- First check the CRC:
               declare
                  use Byte_Array_Pointer;
                  use Byte_Array_Pointer.Packed;
                  use Basic_Types;
                  -- Extract the parameter table header:
                  Ptr : constant Byte_Array_Pointer.Instance := Unpack (Arg.Region);
                  Ptr_Header : constant Byte_Array_Pointer.Instance := Slice (Ptr, Start_Index => 0, End_Index => Parameter_Table_Header.Size_In_Bytes - 1);
                  Table_Header : constant Parameter_Table_Header.T := Parameter_Table_Header.Serialization.From_Byte_Array (To_Byte_Array (Ptr_Header));
                  -- Compute the CRC over the incoming table:
                  Computed_Crc : constant Crc_16.Crc_16_Type := Self.Crc_Parameter_Table (To_Byte_Array (Ptr));
               begin
                  -- If the CRCs match, then update the downstream components' internal parameters:
                  if Table_Header.Crc_Table = Computed_Crc then
                     -- Save off crc and version:
                     Self.Table_Version := Table_Header.Version;
                     Self.Stored_Crc := Table_Header.Crc_Table;
                     -- Update the parameter table:
                     To_Return := Self.Update_Parameter_Table (Arg.Region);
                  else
                     -- If the CRCs do not match then throw an event and do NOT update the downstream components'
                     -- internal parameters:
                     Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Crc_Invalid (Self.Sys_Time_T_Get, (Parameters_Region => Arg, Header => Table_Header, Computed_Crc => Computed_Crc)));
                     To_Return := (Region => Arg.Region, Status => Crc_Error);
                  end if;
               end;
               -- The memory region needs to be filled by the current values of all our active parameters:
            when Get =>
               To_Return := Self.Copy_Parameter_Table_To_Region (Arg.Region);
         end case;
      end if;

      -- Return the memory pointer with the status for deallocation.
      Self.Parameters_Memory_Region_Release_T_Send_If_Connected (To_Return);
   end Parameters_Memory_Region_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Memory_Region_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      use Parameter_Enums.Parameter_Table_Update_Status;
   begin
      -- Even through the memory region was dropped, we still need to release it:
      Self.Parameters_Memory_Region_Release_T_Send_If_Connected ((Region => Arg.Region, Status => Dropped));

      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Dropped (Self.Sys_Time_T_Get, Arg));
   end Parameters_Memory_Region_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Parameters component.
   -- Update the active parameter value in a component for a parameter with the given ID, Length, and Value.
   overriding function Update_Parameter (Self : in out Instance; Arg : in Parameter.T) return Command_Execution_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Command_Execution_Status;
      use Parameter_Types;
   begin
      -- Search through our entries linearly to see if a parameter with the id is found:
      for Idx in Self.Entries.all'Range loop
         declare
            Param_Entry : Parameters_Component_Types.Parameter_Table_Entry renames Self.Entries.all (Idx);
         begin
            -- See if this entry contains the ID we are looking for.
            if Arg.Header.Id = Param_Entry.Id then
               declare
                  -- Calculate expected parameter length:
                  Param_Length : constant Parameter_Types.Parameter_Buffer_Length_Type := Param_Entry.End_Index - Param_Entry.Start_Index + 1;
               begin
                  -- See if the length in the header is what we expect.
                  if Arg.Header.Buffer_Length /= Param_Length then
                     Self.Event_T_Send_If_Connected (Self.Events.Parameter_Update_Length_Mismatch (Self.Sys_Time_T_Get, (
                        Header => Arg.Header,
                        Expected_Length => Param_Length)
                     ));
                     return Failure;
                  else
                     -- OK everything looks good, let's stage the parameter.
                     if Self.Stage_Parameter (Param_Entry => Param_Entry, Value => Arg.Buffer) /= Success then
                        -- No need to throw an event, because an event is thrown in the function above
                        -- if something doesn't go well.
                        return Failure;
                     end if;

                     -- Now let's update the parameter.
                     if Self.Update_Parameters (Component_Id => Param_Entry.Component_Id) /= Success then
                        -- No need to throw an event, because an event is thrown in the function above
                        -- if something doesn't go well.
                        return Failure;
                     end if;

                     -- Send out a new parameter's packet if configured to do so:
                     if Self.Dump_Parameters_On_Change then
                        declare
                           -- Just call the dump parameter's command handler.
                           Stat : constant Command_Execution_Status.E := Self.Dump_Parameters;
                        begin
                           if Stat /= Success then
                              return Stat;
                           end if;
                        end;
                     end if;

                     -- If we got here than everything worked as expected:
                     Self.Event_T_Send_If_Connected (Self.Events.Parameter_Update_Success (Self.Sys_Time_T_Get, (Id => Arg.Header.Id)));
                     return Success;
                  end if;
               end;
            end if;
         end;
      end loop;

      -- If we get here than we did not find an ID that matches the parameter we are asked to update.
      Self.Event_T_Send_If_Connected (Self.Events.Parameter_Update_Id_Not_Recognized (Self.Sys_Time_T_Get, (Id => Arg.Header.Id)));
      return Failure;
   end Update_Parameter;

   -- Produce a packet with the currently staged parameter values contained within connected components.
   overriding function Dump_Parameters (Self : in out Instance) return Command_Execution_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Command_Execution_Status;
   begin
      -- Send the parameters packet and return a status based on if it was completely successful or not.
      if Self.Send_Parameters_Packet /= Success then
         -- Set failure return since something went wrong. A more specific event will be thrown by
         -- the Send_Parameters_Packet function.
         return Failure;
      end if;

      return Success;
   end Dump_Parameters;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Perform action to handle an invalid command.
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Parameters.Implementation;
