--------------------------------------------------------------------------------
-- Parameter_Store Component Implementation Body
--------------------------------------------------------------------------------

with Parameter_Enums;
with Byte_Array_Pointer.Packed;
with Packet_Types;
with Serializer_Types;
with Parameter_Table_Header;
with Parameter_Table_Util;
with Memory_Region;
with Crc_16;

package body Component.Parameter_Store.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory region it is to manage which holds the parameter table.
   --
   -- Init Parameters:
   -- bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to be used for storing the parameter table. The size of this byte array MUST be the exact size of the parameter table to be stored for Set operations, or updating the table will be rejected with a length error. For Get operations, the provided memory region may be larger than or equal to the table size.
   -- Dump_Parameters_On_Change : Boolean - If set to True, the component will dump the current parameter values any time a memory region is received to change the parameter table. If set to False, parameters will only be dumped when requested by command.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in not null Basic_Types.Byte_Array_Access; Dump_Parameters_On_Change : in Boolean := False) is
   begin
      Self.Bytes := Bytes;
      Self.Dump_Parameters_On_Change := Dump_Parameters_On_Change;
   end Init;

   --------------------------------------------------
   -- Set_Up: validate dump-pathway wiring.
   --------------------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is
      Packet_Wired : constant Boolean := Self.Is_Packet_T_Send_Connected;
      Memory_Wired : constant Boolean := Self.Is_Memory_Dump_Send_Connected;
   begin
      -- At most one dump path may be wired. Both connected is a config
      -- error (ambiguous). Neither connected silences the Dump command.
      pragma Assert (not (Packet_Wired and then Memory_Wired));
      -- The Packet.T path is constrained such that table + CRC prefix must
      -- fit in one Packet.T. The Memory_Dump path has no such constraint (the
      -- downstream packetizer chunks the table which can be larger than a
      -- single packet).
      if Packet_Wired then
         pragma Assert (Self.Bytes.all'Length + Crc_16.Crc_16_Type'Length <= Packet_Types.Packet_Buffer_Type'Length);
      end if;
   end Set_Up;

   -- Helper function to construct and send a parameters packet filled with
   -- the parameter table data. Branches on the wired dump pathway:
   --   * Packet.T path: build a single Stored_Parameters packet (Computed_Crc
   --     prefix + bytes) and send it. Set_Up already asserted the size fits.
   --   * Memory_Dump path: emit a single Memory_Dump record with the table
   --     bytes pointer; a downstream Memory_Packetizer or similar component
   --     chunks the region into Packet.T's.
   procedure Send_Parameters_Packet (Self : in out Instance) is
   begin
      if Self.Is_Packet_T_Send_Connected then
         -- First calculate the CRC of the parameter store, and prepend it to
         -- the packet payload. We do this every time we dump the packet, to
         -- make sure the calculated CRC is always up to date. This lets the
         -- ground see if a bit flip occurred in the managed bytes.
         declare
            use Serializer_Types;
            use Basic_Types;
            -- Compute the CRC over the parameter table bytes:
            Computed_Crc : constant Crc_16.Crc_16_Type :=
               Parameter_Table_Util.Compute_Table_Crc (
                  Byte_Array_Pointer.From_Address (Self.Bytes.all'Address, Self.Bytes.all'Length));
            -- Create the packet with the CRC prefix inserted:
            Pkt : Packet.T;
            Stat : constant Serialization_Status :=
               Self.Packets.Stored_Parameters (Self.Sys_Time_T_Get, Computed_Crc & Self.Bytes.all, Pkt);
         begin
            -- This should never fail since Set_Up asserted that self.bytes.all
            -- could fit cleanly within a Packet.T type along with the CRC.
            pragma Assert (Stat = Success);
            -- Send the packet:
            Self.Packet_T_Send (Pkt);
         end;
         Self.Event_T_Send_If_Connected (Self.Events.Dumped_Parameters (Self.Sys_Time_T_Get));
      elsif Self.Is_Memory_Dump_Send_Connected then
         Self.Memory_Dump_Send ((
            Id => Self.Packets.Get_Stored_Parameters_Id,
            Memory_Pointer => Byte_Array_Pointer.Packed.Unpack ((
               Address => Self.Bytes.all'Address,
               Length => Self.Bytes.all'Length
            ))
         ));
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
      -- Perform operation-specific length checks and processing:
      case Arg.Operation is
         -- The memory region contains a fresh parameter table. We need to use this parameter table to
         -- update the parameter store. The region must be exactly the expected size.
         when Set =>
            if Arg.Region.Length /= Self.Bytes.all'Length then
               Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (
                  Parameters_Region => Arg,
                  Expected_Length => Self.Bytes.all'Length)
               ));
               To_Return := (Region => Arg.Region, Status => Length_Error);
            else
               declare
                  use Byte_Array_Pointer;
                  use Basic_Types;
                  Table_Header : Parameter_Table_Header.T;
                  Ptr : constant Byte_Array_Pointer.Instance := Parameter_Table_Util.Get_Ptr_And_Header_From_Region (Arg.Region, Table_Header);
                  -- Compute the CRC over the incoming table. Arg.Region.Length was
                  -- checked equal to Self.Bytes.all'Length above, so the Ptr spans
                  -- exactly the managed table length.
                  pragma Assert (Length (Ptr) = Self.Bytes.all'Length);
                  Computed_Crc : constant Crc_16.Crc_16_Type := Parameter_Table_Util.Compute_Table_Crc (Ptr);
               begin
                  -- If the CRCs match, then update the store:
                  if Table_Header.Crc_Table = Computed_Crc then
                     -- Update the parameter store. Use the procedural Copy_From
                     -- rather than the function-form To_Byte_Array; the latter
                     -- returns an unconstrained Byte_Array on the secondary
                     -- stack, which overflows for tables larger than the task's
                     -- secondary-stack budget (e.g. ~10 KB OE tables blow a
                     -- 3 KB secondary stack). Copy_From writes in place with
                     -- no temporary.
                     Byte_Array_Pointer.Copy_From (Ptr, Self.Bytes.all);
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
            end if;
         -- The memory region needs to be filled by the current values in our parameter store.
         -- The provided region must be at least as large as the parameter table. If it is larger,
         -- only the table size worth of bytes are copied and the returned region length is updated
         -- to reflect the actual number of bytes written.
         when Get_Copy =>
            if Arg.Region.Length < Self.Bytes.all'Length then
               Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (
                  Parameters_Region => Arg,
                  Expected_Length => Self.Bytes.all'Length)
               ));
               To_Return := (Region => Arg.Region, Status => Length_Error);
            else
               declare
                  use Byte_Array_Pointer;
                  Ptr : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Packed.Unpack (Arg.Region);
                  -- Slice the pointer to the exact size of the parameter table, in case the
                  -- provided region is larger:
                  Dest : constant Byte_Array_Pointer.Instance := Slice (Ptr, Start_Index => 0, End_Index => Self.Bytes.all'Length - 1);
                  -- Build the return region with the actual number of bytes copied:
                  Returned_Region : constant Memory_Region.T := (Address => Arg.Region.Address, Length => Self.Bytes.all'Length);
               begin
                  -- Copy memory into the sliced destination:
                  Byte_Array_Pointer.Copy_To (Dest, Self.Bytes.all);
                  -- Send info event with the adjusted region length:
                  Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Fetched (Self.Sys_Time_T_Get, Returned_Region));
                  -- Return the region with the updated length:
                  To_Return := (Region => Returned_Region, Status => Success);
               end;
            end if;
         -- Return a memory region pointing at the parameter store's own
         -- byte buffer (zero-copy). The caller-provided region in Arg is
         -- ignored. The returned region is read-valid only until the next
         -- Set arrives that overwrites the buffer; the caller is
         -- responsible for consuming it before issuing more writes.
         when Get_Pointer =>
            declare
               Returned_Region : constant Memory_Region.T :=
                  (Address => Self.Bytes.all'Address, Length => Self.Bytes.all'Length);
            begin
               Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Pointer_Fetched (Self.Sys_Time_T_Get, Returned_Region));
               To_Return := (Region => Returned_Region, Status => Success);
            end;
         when Validate =>
            -- This component does not perform component-specific validation, so table validation is unsupported:
            -- Throw event:
            Self.Event_T_Send_If_Connected (Self.Events.Table_Validation_Not_Supported (Self.Sys_Time_T_Get, Arg.Region));
            -- Set the return status:
            To_Return := (Region => Arg.Region, Status => Parameter_Error);
      end case;

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
