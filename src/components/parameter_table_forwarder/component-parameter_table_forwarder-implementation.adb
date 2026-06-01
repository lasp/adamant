--------------------------------------------------------------------------------
-- Parameter_Table_Forwarder Component Implementation Body
--------------------------------------------------------------------------------

with Byte_Array_Pointer;
with Byte_Array_Pointer.Packed;
with Crc_16;
with Memory_Region;
with Parameter_Enums;
with Parameter_Table_Util;
with System;
with System.Storage_Elements;

package body Component.Parameter_Table_Forwarder.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   overriding procedure Init (Self : in out Instance; Table_Size : in Natural; Dump_Parameters_On_Change : in Boolean := False) is
   begin
      -- Table_Size must be larger than the parameter table header.
      pragma Assert (Table_Size > Parameter_Table_Header.Size_In_Bytes);
      Self.Table_Size := Table_Size;
      Self.Dump_On_Change := Dump_Parameters_On_Change;
   end Init;

   ---------------------------------------
   -- Helper functions:
   ---------------------------------------

   -- Slice the header off an upstream memory region, returning a region that
   -- points at the payload bytes only. The downstream component only sees
   -- payload; the forwarder is solely responsible for constructing /
   -- validating the header.
   function Payload_Region_From (Self : in Instance; Full : in Memory_Region.T) return Memory_Region.T is
      use System.Storage_Elements;
   begin
      return (
         Address => Full.Address + Storage_Offset (Parameter_Table_Header.Size_In_Bytes),
         Length => Self.Table_Size - Parameter_Table_Header.Size_In_Bytes
      );
   end Payload_Region_From;

   -- Length-check the incoming region, decode + verify the header CRC, and
   -- emit the appropriate Length_Error / Crc_Error event on failure.
   -- Returns Success and populates Table_Header on success; returns the
   -- error status on failure. Shared by Set and Validate, which have the
   -- same length+CRC validation prefix but diverge on the downstream-
   -- forward tail.
   function Validate_Length_And_Crc (
      Self : in out Instance;
      Arg : in Parameters_Memory_Region.T;
      Table_Header : out Parameter_Table_Header.T
   ) return Parameter_Enums.Parameter_Table_Update_Status.E is
      use Parameter_Enums.Parameter_Table_Update_Status;
   begin
      if Arg.Region.Length /= Self.Table_Size then
         Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (
            Parameters_Region => Arg,
            Expected_Length => Self.Table_Size
         )));
         return Length_Error;
      else
         declare
            use Basic_Types;
            Ptr : constant Byte_Array_Pointer.Instance := Parameter_Table_Util.Get_Ptr_And_Header_From_Region (Arg.Region, Table_Header);
            Computed_Crc : constant Crc_16.Crc_16_Type := Parameter_Table_Util.Compute_Table_Crc (Ptr);
         begin
            if Table_Header.Crc_Table /= Computed_Crc then
               Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Crc_Invalid (Self.Sys_Time_T_Get, (
                  Parameters_Region => Arg,
                  Header => Table_Header,
                  Computed_Crc => Computed_Crc
               )));
               return Crc_Error;
            end if;
         end;
         return Success;
      end if;
   end Validate_Length_And_Crc;

   -- Write the parameter table header bytes (from the forwarder's
   -- Stored_Header) into the first Parameter_Table_Header.Size_In_Bytes
   -- bytes of the given upstream region. Called after a successful
   -- Get_Copy-from-downstream so the upstream caller sees a fully-formed
   -- table. The downstream only fills the payload because the forwarder
   -- strips the header on every inbound Set/Validate.
   procedure Write_Stored_Header_To (Self : in Instance; Region : in Memory_Region.T) is
      use Byte_Array_Pointer;
      Header_Bytes : constant Basic_Types.Byte_Array :=
         Parameter_Table_Header.Serialization.To_Byte_Array (Self.Stored_Header);
      Ptr : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Packed.Unpack (Region);
      Header_Slice : constant Byte_Array_Pointer.Instance :=
         Slice (Ptr, Start_Index => 0, End_Index => Parameter_Table_Header.Size_In_Bytes - 1);
   begin
      Byte_Array_Pointer.Copy_To (Header_Slice, Header_Bytes);
   end Write_Stored_Header_To;

   -- Build and send an Active_Parameters dump by issuing a Get_Pointer to
   -- the downstream component and emitting two Memory_Dump records (header
   -- + payload) on the Memory_Dump_Send connector. A downstream
   -- Memory_Packetizer, or similar component, can chunk each region into
   -- Packet.T's.
   --
   -- Returns the downstream's Get_Pointer status.
   function Send_Active_Parameters_Packet (Self : in out Instance) return Parameter_Enums.Parameter_Table_Update_Status.E is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;

      -- Callers must check Is_Memory_Dump_Send_Connected before invoking this,
      -- so the unconditional Memory_Dump_Send calls below are safe.
      pragma Assert (Self.Is_Memory_Dump_Send_Connected);

      Release : constant Parameters_Memory_Region_Release.T := Self.Parameters_Memory_Region_T_Forwarded ((
         Region => (Address => System.Null_Address, Length => 0),
         Operation => Get_Pointer
      ));
   begin
      if Release.Status = Success then
         -- Perform a two-region dump under the same Active_Parameters packet ID:
         -- the forwarder's Stored_Header (a packed record whose in-memory
         -- bytes match the wire format) and the payload region returned by
         -- the downstream's Get_Pointer.
         declare
            Active_Parameters_Id : constant Packet_Types.Packet_Id :=
               Self.Packets.Get_Active_Parameters_Id;
         begin
            Self.Memory_Dump_Send ((
               Id => Active_Parameters_Id,
               Memory_Pointer => Byte_Array_Pointer.From_Address (
                  Self.Stored_Header'Address, Parameter_Table_Header.Size_In_Bytes
               )
            ));
            Self.Memory_Dump_Send ((
               Id => Active_Parameters_Id,
               Memory_Pointer => Byte_Array_Pointer.Packed.Unpack (Release.Region)
            ));
         end;
         Self.Event_T_Send_If_Connected (Self.Events.Dumped_Parameters (Self.Sys_Time_T_Get));
      end if;

      return Release.Status;
   end Send_Active_Parameters_Packet;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Command receive.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   overriding procedure Parameters_Memory_Region_T_Recv_Async (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      To_Return : Parameters_Memory_Region_Release.T := (Region => Arg.Region, Status => Success);
   begin
      case Arg.Operation is
         ----------------------------------------------------------------
         -- Set: validate length + CRC, slice header off and forward the
         -- payload region to the downstream, optional dump on change.
         ----------------------------------------------------------------
         when Set =>
            declare
               Table_Header : Parameter_Table_Header.T;
               Pre_Status : constant Parameter_Enums.Parameter_Table_Update_Status.E :=
                  Self.Validate_Length_And_Crc (Arg, Table_Header);
            begin
               if Pre_Status /= Success then
                  To_Return := (Region => Arg.Region, Status => Pre_Status);
               else
                  -- Forward only the payload (header stripped). The request
                  -- returns the downstream's release status synchronously.
                  declare
                     Downstream_Status : constant Parameter_Enums.Parameter_Table_Update_Status.E :=
                        Self.Parameters_Memory_Region_T_Forwarded ((
                           Region => Self.Payload_Region_From (Arg.Region),
                           Operation => Set
                        )).Status;
                  begin
                     if Downstream_Status = Success then
                        Self.Stored_Header := Table_Header;
                        Self.Table_Update_Time := Self.Sys_Time_T_Get.Seconds;
                        Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Updated (Self.Sys_Time_T_Get, Arg.Region));
                        -- Auto-dump after a successful Set. Only attempt it when a
                        -- dump pathway is actually wired; an unconnected Memory_Dump
                        -- connector behaves the same as Dump_On_Change => False. If
                        -- the dump itself fails (e.g. downstream Get_Pointer
                        -- rejected), emit Dump_Failed and propagate that status to
                        -- the upstream caller.
                        if Self.Dump_On_Change and then Self.Is_Memory_Dump_Send_Connected then
                           declare
                              Dump_Status : constant Parameter_Enums.Parameter_Table_Update_Status.E := Self.Send_Active_Parameters_Packet;
                           begin
                              if Dump_Status /= Success then
                                 Self.Event_T_Send_If_Connected (Self.Events.Dump_Failed (
                                    Self.Sys_Time_T_Get,
                                    (Region => Arg.Region, Status => Dump_Status)
                                 ));
                              end if;
                              To_Return := (Region => Arg.Region, Status => Dump_Status);
                           end;
                        else
                           To_Return := (Region => Arg.Region, Status => Success);
                        end if;
                     else
                        Self.Event_T_Send_If_Connected (Self.Events.Downstream_Component_Rejected_Update (
                           Self.Sys_Time_T_Get,
                           (Region => Arg.Region, Status => Downstream_Status)
                        ));
                        To_Return := (Region => Arg.Region, Status => Downstream_Status);
                     end if;
                  end;
               end if;
            end;

         ----------------------------------------------------------------
         -- Validate: validate length + CRC, slice header off and forward
         -- the payload region. Never updates Stored_Crc/Version/Time.
         ----------------------------------------------------------------
         when Validate =>
            declare
               Table_Header : Parameter_Table_Header.T;
               Pre_Status : constant Parameter_Enums.Parameter_Table_Update_Status.E :=
                  Self.Validate_Length_And_Crc (Arg, Table_Header);
            begin
               if Pre_Status /= Success then
                  To_Return := (Region => Arg.Region, Status => Pre_Status);
               else
                  -- Forward only the payload (header stripped). The request
                  -- returns the downstream's release status synchronously.
                  declare
                     Downstream_Status : constant Parameter_Enums.Parameter_Table_Update_Status.E :=
                        Self.Parameters_Memory_Region_T_Forwarded ((
                           Region => Self.Payload_Region_From (Arg.Region),
                           Operation => Validate
                        )).Status;
                  begin
                     if Downstream_Status = Success then
                        Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Validated (Self.Sys_Time_T_Get, Arg.Region));
                     else
                        Self.Event_T_Send_If_Connected (Self.Events.Downstream_Component_Rejected_Validation (
                           Self.Sys_Time_T_Get,
                           (Region => Arg.Region, Status => Downstream_Status)
                        ));
                     end if;
                     To_Return := (Region => Arg.Region, Status => Downstream_Status);
                  end;
               end if;
            end;

         ----------------------------------------------------------------
         -- Get_Copy: validate length >= Table_Size, forward payload-only
         -- Get_Copy to downstream, then write the stored header into the
         -- upstream region so the caller sees a fully-formed table.
         ----------------------------------------------------------------
         when Get_Copy =>
            if Arg.Region.Length < Self.Table_Size then
               Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Length_Mismatch (Self.Sys_Time_T_Get, (
                  Parameters_Region => Arg,
                  Expected_Length => Self.Table_Size
               )));
               To_Return := (Region => Arg.Region, Status => Length_Error);
            else
               -- Forward Get_Copy for the payload only. Downstream writes
               -- payload bytes; the forwarder writes the header on success
               -- so the caller's region holds a fully-formed table (header
               -- + payload). Headers are forwarder-owned: downstream never
               -- sees them on Set, so downstream can't return them on
               -- Get_Copy either.
               declare
                  Downstream_Status : constant Parameter_Enums.Parameter_Table_Update_Status.E :=
                     Self.Parameters_Memory_Region_T_Forwarded ((
                        Region => Self.Payload_Region_From (Arg.Region),
                        Operation => Get_Copy
                     )).Status;
               begin
                  if Downstream_Status = Success then
                     Self.Write_Stored_Header_To (Arg.Region);
                     declare
                        Returned_Region : constant Memory_Region.T :=
                           (Address => Arg.Region.Address, Length => Self.Table_Size);
                     begin
                        Self.Event_T_Send_If_Connected (Self.Events.Parameter_Table_Fetched (Self.Sys_Time_T_Get, Returned_Region));
                        To_Return := (Region => Returned_Region, Status => Success);
                     end;
                  else
                     Self.Event_T_Send_If_Connected (Self.Events.Downstream_Component_Rejected_Fetch (
                        Self.Sys_Time_T_Get,
                        (Region => Arg.Region, Status => Downstream_Status)
                     ));
                     To_Return := (Region => Arg.Region, Status => Downstream_Status);
                  end if;
               end;
            end if;

         ----------------------------------------------------------------
         -- Get_Pointer: not supported on the external boundary. The
         -- forwarder owns the header and strips it on every inbound
         -- Set/Validate, so it could not insert the header into a
         -- caller-visible region returned by the downstream. Reject the
         -- request; callers wanting a full table must use Get_Copy. The
         -- forwarder's internal Send_Active_Parameters_Packet uses
         -- Get_Pointer against the downstream directly and fuses the
         -- result with Stored_Header into the two-region Memory_Dump.
         ----------------------------------------------------------------
         when Get_Pointer =>
            Self.Event_T_Send_If_Connected (Self.Events.Get_Pointer_Not_Supported (Self.Sys_Time_T_Get, Arg.Region));
            To_Return := (Region => Arg.Region, Status => Parameter_Error);
      end case;

      -- Emit the Table_Status data product on Set/Validate. Get_Copy/Get_Pointer
      -- do not mutate the active-table bookkeeping, so they do not advance
      -- Table_Status. This mirrors the Parameters component.
      if Arg.Operation = Set or else Arg.Operation = Validate then
         Self.Data_Product_T_Send_If_Connected (
            Self.Data_Products.Table_Status (
               Self.Sys_Time_T_Get,
               (Active_Table_Version_Number => Self.Stored_Header.Version,
                Active_Table_Update_Time => Self.Table_Update_Time,
                Active_Table_Crc => Self.Stored_Header.Crc_Table,
                Last_Table_Operation_Status => To_Return.Status)
            )
         );
      end if;

      -- Release back to the upstream sender.
      Self.Parameters_Memory_Region_Release_T_Send_If_Connected (To_Return);
   end Parameters_Memory_Region_T_Recv_Async;

   overriding procedure Parameters_Memory_Region_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      use Parameter_Enums.Parameter_Table_Update_Status;
   begin
      Self.Parameters_Memory_Region_Release_T_Send_If_Connected ((Region => Arg.Region, Status => Dropped));
      Self.Event_T_Send_If_Connected (Self.Events.Memory_Region_Dropped (Self.Sys_Time_T_Get, Arg));
   end Parameters_Memory_Region_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   overriding function Dump_Parameter_Table (Self : in out Instance) return Command_Execution_Status.E is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use System.Storage_Elements;
   begin
      -- No dump pathway wired: the command cannot do anything. Report it with a
      -- Dump_Failed event carrying a recognizable sentinel region (zero length,
      -- 0xDEAD_BEEF address) so the operator can tell the dump connector is
      -- unconnected rather than the downstream having rejected a real dump.
      if not Self.Is_Memory_Dump_Send_Connected then
         Self.Event_T_Send_If_Connected (Self.Events.Dump_Failed (
            Self.Sys_Time_T_Get,
            (Region => (Address => To_Address (16#DEAD_BEEF#), Length => 0), Status => Parameter_Error)
         ));
         return Command_Execution_Status.Failure;
      end if;

      declare
         Status : constant Parameter_Enums.Parameter_Table_Update_Status.E := Self.Send_Active_Parameters_Packet;
      begin
         if Status = Success then
            return Command_Execution_Status.Success;
         else
            Self.Event_T_Send_If_Connected (Self.Events.Dump_Failed (
               Self.Sys_Time_T_Get,
               (Region => (Address => System.Null_Address, Length => Self.Table_Size), Status => Status)
            ));
            return Command_Execution_Status.Failure;
         end if;
      end;
   end Dump_Parameter_Table;

   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Parameter_Table_Forwarder.Implementation;
