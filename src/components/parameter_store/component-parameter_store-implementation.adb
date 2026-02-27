--------------------------------------------------------------------------------
-- Parameter_Store Component Implementation Body
--------------------------------------------------------------------------------

with Parameter_Enums;
with Byte_Array_Pointer.Packed;
with Packet_Types;
with Serializer_Types;
with Parameter_Table_Header;
with Crc_16;

package body Component.Parameter_Store.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory region it is to manage which holds the parameter table.
   --
   -- Init Parameters:
   -- bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to be used for storing the parameter table. The size of this byte array MUST be the exact size of the parameter table to be stored, or updating or fetching the table will be rejected with a length error.
   -- Dump_Parameters_On_Change : Boolean - If set to True, the component will dump the current parameter values any time a memory region is received to change the parameter table. If set to False, parameters will only be dumped when requested by command.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in not null Basic_Types.Byte_Array_Access; Dump_Parameters_On_Change : in Boolean := False) is
   begin
      -- The implementation of this component assumes that the parameter store fits cleanly in a maximum sized packet.
      pragma Assert (Bytes.all'Length + Crc_16.Crc_16_Type'Length <= Packet_Types.Packet_Buffer_Type'Length, "The parameter table must not be larger than the maximum size packet!");
      Self.Bytes := Bytes;
      Self.Dump_Parameters_On_Change := Dump_Parameters_On_Change;
   end Init;

   ---------------------------------------
   -- Helper functions:
   ---------------------------------------
   -- Crc the parameter table bytes. The table bytes passed in MUST be the exact size as the parameter table:
   function Crc_Parameter_Table (Self : in Instance; Table_Bytes : in Basic_Types.Byte_Array) return Crc_16.Crc_16_Type is
   begin
      -- This function assumes that the provided data is the exact length of the parameter table. Length
      -- checks should be performed before calling this function:
      pragma Assert (Table_Bytes'Length = Self.Bytes.all'Length);

      -- Some checks to make sure parameter table header constants make sense. This will fail if the
      -- header packed record and constants are malformed.
      pragma Assert (Parameter_Table_Header.Crc_Section_Length + Parameter_Table_Header.Version_Length = Parameter_Table_Header.Size_In_Bytes);

      -- Calculate the CRC over the version and data:
      return Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
   end Crc_Parameter_Table;

   -- Helper function to construct and send a parameters packet filled with the parameter
   -- table data.
   procedure Send_Parameters_Packet (Self : in out Instance) is
   begin
      -- Only execute this logic if the packet connector is connected.
      if Self.Is_Packet_T_Send_Connected then
         -- First calculate the CRC of the parameter store, and overwrite the calculated_CRC field in the header.
         -- We do this every time we dump the packet, to make sure the calculated_CRC is always up to date. This
         -- lets the ground see if a bit flip occurred.
         declare
            use Serializer_Types;
            use Basic_Types;
            -- Compute the CRC over the table:
            Computed_Crc : constant Crc_16.Crc_16_Type := Self.Crc_Parameter_Table (Self.Bytes.all);
            -- Create the packet with the CRC calc inserted:
            Pkt : Packet.T;
            Stat : constant Serialization_Status := Self.Packets.Stored_Parameters (Self.Sys_Time_T_Get, Computed_Crc & Self.Bytes.all, Pkt);
         begin
            -- Send the packet:
            pragma Assert (Stat = Success, "This should never fail since we checked at Init that self.bytes.all could fit cleanly within a Packet.T type.");
            Self.Packet_T_Send (Pkt);
         end;

         -- Send event:
         Self.Event_T_Send_If_Connected (Self.Events.Dumped_Parameters (Self.Sys_Time_T_Get));
      end if;
   end Send_Parameters_Packet;

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

   -- When a memory region is received on this connector it is assumed that it contains a memory region that is the same size as the managed region?
   overriding procedure Parameters_Memory_Region_T_Recv_Async (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      To_Return : Parameters_Memory_Region_Release.T := (Region => Arg.Region, Status => Success);
   begin
      -- First make sure that the memory region is of the expected size. If it is not then we will
      -- reject this request.
      if Arg.Region.Length /= Self.Bytes.all'Length then
         Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (
            Parameters_Region => Arg,
            Expected_Length => Self.Bytes.all'Length)
         ));
         To_Return := (Region => Arg.Region, Status => Length_Error);
      else
         declare
            use Byte_Array_Pointer;
            -- Extract the parameter table header:
            Ptr : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Packed.Unpack (Arg.Region);
         begin
            -- Perform the requested operation:
            case Arg.Operation is
               -- The memory region contains a fresh parameter table. We need to use this parameter table to
               -- update the parameter store.
               when Set =>
                  -- First check the CRC:
                  declare
                     use Basic_Types;
                     Ptr_Header : constant Byte_Array_Pointer.Instance := Slice (Ptr, Start_Index => 0, End_Index => Parameter_Table_Header.Size_In_Bytes - 1);
                     Table_Header : constant Parameter_Table_Header.T := Parameter_Table_Header.Serialization.From_Byte_Array (To_Byte_Array (Ptr_Header));
                     -- Compute the CRC over the incoming table:
                     Computed_Crc : constant Crc_16.Crc_16_Type := Self.Crc_Parameter_Table (To_Byte_Array (Ptr));
                  begin
                     -- If the CRCs match, then update the store:
                     if Table_Header.Crc_Table = Computed_Crc then
                        -- Update the parameter store:
                        Self.Bytes.all := Byte_Array_Pointer.To_Byte_Array (Ptr);
                        -- Send info event:
                        Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Updated (Self.Sys_Time_T_Get, Arg.Region));
                        -- Send out the parameters packet if necessary:
                        if Self.Dump_Parameters_On_Change then
                           Self.Send_Parameters_Packet;
                        end if;
                     else
                        -- If the CRCs do not match then throw an event and do NOT update the downstream components'
                        -- internal parameters:
                        Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Crc_Invalid (Self.Sys_Time_T_Get, (
                           Parameters_Region => Arg,
                           Header => Table_Header,
                           Computed_Crc => Computed_Crc)
                        ));
                        -- Alter the return status to reflect a CRC error.
                        To_Return := (Region => Arg.Region, Status => Crc_Error);
                     end if;
                  end;
                  -- The memory region needs to be filled by the current values in our parameter store:
               when Get =>
                  -- Copy memory:
                  Byte_Array_Pointer.Copy_To (Ptr, Self.Bytes.all);
                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Fetched (Self.Sys_Time_T_Get, Arg.Region));
               when Validate =>
                  -- This component does not perform component-specific validation, so table validation is unsupported:
                  -- Throw event:
                  Self.Event_T_Send_If_Connected (Self.Events.Table_Validation_Not_Supported (Self.Sys_Time_T_Get, Arg.Region));
                  -- Set the return status:
                  To_Return := (Region => Arg.Region, Status => Parameter_Error);
            end case;
         end;
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

   -- This procedure is called when a Parameters_Memory_Region_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      use Parameter_Enums.Parameter_Table_Update_Status;
   begin
      -- Even though the memory region was dropped, we still need to release it:
      Self.Parameters_Memory_Region_Release_T_Send_If_Connected ((Region => Arg.Region, Status => Dropped));

      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Dropped (Self.Sys_Time_T_Get, Arg));
   end Parameters_Memory_Region_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Parameter Store component.
   -- Produce a packet with the currently stored parameter values.
   overriding function Dump_Parameter_Store (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Send_Parameters_Packet;
      return Success;
   end Dump_Parameter_Store;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Parameter_Store.Implementation;
