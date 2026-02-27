--------------------------------------------------------------------------------
-- Memory_Dumper Component Implementation Body
--------------------------------------------------------------------------------

with Memory_Manager_Types;
with Byte_Array_Pointer;
with Crc_16;

package body Component.Memory_Dumper.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of memory regions which it can dump and CRC.
   --
   -- Init Parameters:
   -- Memory_Regions : Memory_Manager_Types.Memory_Region_Array_Access - An access to a list of memory regions.
   --
   overriding procedure Init (Self : in out Instance; Memory_Regions : in not null Memory_Manager_Types.Memory_Region_Array_Access) is
   begin
      Self.Regions := Memory_Regions;
   end Init;

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

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Dumper component.
   -- Dump a region of memory starting at a given address and of a given length.
   overriding function Dump_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Ptr : Byte_Array_Pointer.Instance;
      Ignore : Natural;
   begin
      if Memory_Manager_Types.Is_Region_Valid (Arg, Self.Regions, Ptr, Ignore) then
         Self.Event_T_Send_If_Connected (Self.Events.Dumping_Memory (Self.Sys_Time_T_Get, Arg));
         Self.Memory_Dump_Send ((Id => Self.Packets.Get_Memory_Dump_Packet_Id, Memory_Pointer => Ptr));
         return Success;
      else
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Memory_Region (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;
   end Dump_Memory;

   -- Perform a CRC on a region of memory starting at a given address and of a given length. The CRC will be reported via event and data product, if those connectors are connected.
   overriding function Crc_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Ptr : Byte_Array_Pointer.Instance;
      Crc : Crc_16.Crc_16_Type;
      Ignore : Natural;
   begin
      if Memory_Manager_Types.Is_Region_Valid (Arg, Self.Regions, Ptr, Ignore) then
         Self.Event_T_Send_If_Connected (Self.Events.Crcing_Memory (Self.Sys_Time_T_Get, Arg));
         -- Calculate CRC:
         Crc := Crc_16.Compute_Crc_16 (Ptr);
         -- Report CRC:
         declare
            Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
         begin
            Self.Event_T_Send_If_Connected (Self.Events.Memory_Crc (Time, (Region => (Address => Arg.Address, Length => Arg.Length), Crc => Crc)));
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Crc_Report (Time, (Region => (Address => Arg.Address, Length => Arg.Length), Crc => Crc)));
         end;
         return Success;
      else
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Memory_Region (Self.Sys_Time_T_Get, Arg));
         return Failure;
      end if;
   end Crc_Memory;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Perform action to handle an invalid command.
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number =>
         Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Memory_Dumper.Implementation;
