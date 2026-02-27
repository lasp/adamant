--------------------------------------------------------------------------------
-- Memory_Manager Component Implementation Body
--------------------------------------------------------------------------------

with System.Storage_Elements;
with Virtual_Memory_Region;
with Byte_Array_Pointer.Packed;
with Crc_16;

package body Component.Memory_Manager.Implementation is

   -- Protected type to manage access to the memory region. This ensures thread safety should
   -- more than one task request or release the memory region at the same time.
   protected body Protected_Memory_Arbiter is

      -- Request access. If status is set to true then a unique ID is returned.
      procedure Request (Self : in out Instance; Id : out Unsigned_16; State : out Memory_Manager_Enums.Memory_State.E; Status : out Request_Status) is
         use Memory_Manager_Enums.Memory_State;
      begin
         -- Do the correct thing based on the current state of the memory.
         case Current_State is
            when Available =>
               -- Set the status:
               Status := Success;
               -- Set the ID:
               Id := Current_Id;
               Current_Id := @ + 1;
               -- Set the new state, the memory is now in use.
               Current_State := In_Use;
               -- Update the data product. This needs to be done inside the protected
               -- object in order to ensure a race condition does not make the data product
               -- invalid:
               Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Memory_Region_Status (Self.Sys_Time_T_Get, (State => Current_State)));
            when In_Use =>
               -- Set the status:
               Status := Memory_Unavailable;
               -- Set the id to something unlikely to
               -- work if ever used on accident:
               Id := Unsigned_16'Last;
         end case;
         State := Current_State;
      end Request;

      -- Release access with a given ID. If status is set to true then the release
      -- succeeded, otherwise an unexpected ID was returned.
      procedure Release (Self : in out Instance; Id : in Unsigned_16; State : out Memory_Manager_Enums.Memory_State.E; Status : out Release_Status) is
         use Memory_Manager_Enums.Memory_State;
         Expected_Id : constant Unsigned_16 := Current_Id - 1;
      begin
         -- Do the correct thing based on the current state of the memory.
         case Current_State is
            when Available =>
               -- A release when the memory is currently available is an error.
               Status := Memory_Available;
            when In_Use =>
               -- Make sure the ID is correct, otherwise return an error and
               -- do not make the memory available.
               if Id = Expected_Id then
                  -- Set the status:
                  Status := Success;
                  -- Set the new state, releasing memory:
                  Current_State := Available;
                  -- Update the data product. This needs to be done inside the protected
                  -- object in order to ensure a race condition does not make the data product
                  -- invalid:
                  Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Memory_Region_Status (Self.Sys_Time_T_Get, (State => Current_State)));
               else
                  -- Set the status to unexpected ID and do not release
                  -- the memory.
                  Status := Unexpected_Id;
               end if;
         end case;
         State := Current_State;
      end Release;

      -- Release access, no ID needed.
      procedure Force_Release (Self : in out Instance) is
         use Memory_Manager_Enums.Memory_State;
      begin
         -- Make the memory available, no questions asked.
         Current_State := Available;
         Current_Id := 0;
         -- Update the data product. This needs to be done inside the protected
         -- object in order to ensure a race condition does not make the data product
         -- invalid:
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Memory_Region_Status (Self.Sys_Time_T_Get, (State => Current_State)));
      end Force_Release;

      function Get_State return Memory_Manager_Enums.Memory_State.E is
      begin
         return Current_State;
      end Get_State;

      function Get_Current_Id return Unsigned_16 is
      begin
         return Current_Id;
      end Get_Current_Id;

   end Protected_Memory_Arbiter;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This init function provides memory allocation for the managers internal memory region. Preallocated memory can be provided via the "bytes" access type, in which case "size" must be negative and will be ignored. If you would like to allocate the internal memory on the heap then "bytes" must be set to null, and "size" must be a positive number representing the number of bytes you would like to allocate.
   --
   -- Init Parameters:
   -- bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to be used for the memory region. If this is set to null, then memory will be allocated on the heap using the "size" parameter instead. Note: This must be set to null if the "size" parameter is positive below.
   -- size : Integer - The number of bytes to allocate on the heap for the memory region. Note: This must be set to a negative value if the "bytes" parameters is not null.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access := null; Size : in Integer := -1) is
      use Basic_Types;
   begin
      -- Set the internal memory:
      if Bytes = null then
         -- If bytes is null make sure size is positive.
         pragma Assert (Size > 0, "Memory Manager Init Error: If a null 'bytes' pointer is provided, then a positive size must be provided to allocate memory on the heap.");
         -- Allocate the internal byte buffer:
         Self.Bytes := new Basic_Types.Byte_Array (0 .. Size - 1);
      else
         -- If bytes is non-null make sure size is negative.
         pragma Assert (Size < 0, "Memory Manager Init Error: If a non null 'bytes' pointer is provided, then a negative size must be provided prevent allocation of memory on the heap.");
         pragma Assert (Bytes'Length > 0, "Length of bytes must be positive.");
         -- Set the internal byte buffer:
         Self.Bytes := Bytes;
      end if;

      -- Initialize the memory region, we will only ever change the ID from now on.
      -- Note: These are basically constants (although that is not enforceable). These
      -- values are set here and should never be changed, only used as necessary
      -- within the component.
      Self.Region := (Address => Self.Bytes.all'Address, Length => Self.Bytes.all'Length);
      Self.Virtual_Region := (Address => 0, Length => Self.Bytes.all'Length);
   end Init;

   -- Use set up procedure to update initial data products:
   overriding procedure Set_Up (Self : in out Instance) is
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Update the data products:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Memory_Region_Status (The_Time, (State => Self.Arbiter.Get_State)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Crc_Report (The_Time, (Region => (Address => 0, Length => 0), Crc => [0, 0])));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Memory_Location (The_Time, Self.Region));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The memory region is requested on this connector.
   overriding function Memory_Region_Request_T_Return (Self : in out Instance) return Memory_Region_Request.T is
      use System.Storage_Elements;
      use Memory_Manager_Enums.Memory_Request_Status;
      Stat : Request_Status;
      Ignore_State : Memory_Manager_Enums.Memory_State.E;
      Id : Unsigned_16;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Request access:
      Self.Arbiter.Request (Self, Id => Id, State => Ignore_State, Status => Stat);

      -- Check status and return memory region or error:
      case Stat is
         when Success =>
            -- Return the memory region with the returned ID:
            return (Ided_Region => (Id => Id, Region => Self.Region), Status => Success);
         when Memory_Unavailable =>
            -- Throw event.
            Self.Event_T_Send_If_Connected (Self.Events.Memory_Unavailable (The_Time));
            -- Return a null address with failure status.
            return (Ided_Region => (Id => 0, Region => (Address => To_Address (Integer_Address (0)), Length => 0)), Status => Failure);
      end case;
   end Memory_Region_Request_T_Return;

   overriding procedure Ided_Memory_Region_T_Release (Self : in out Instance; Arg : in Ided_Memory_Region.T) is
      Stat : Release_Status;
      Ignore_State : Memory_Manager_Enums.Memory_State.E;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Release region:
      Self.Arbiter.Release (Self, Id => Arg.Id, State => Ignore_State, Status => Stat);

      case Stat is
         when Success =>
            -- All is right in the world:
            null;
         when Memory_Available =>
            -- Throw event:
            Self.Event_T_Send_If_Connected (Self.Events.Memory_Already_Released (The_Time, Arg));
         when Unexpected_Id =>
            -- Throw event:
            Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Memory_Id (The_Time, Arg));
      end case;
   end Ided_Memory_Region_T_Release;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Manager component.
   -- Dump the entire memory region.
   overriding function Dump_Memory_Region (Self : in out Instance) return Command_Execution_Status.E is
   begin
      -- Simply call the dump memory region bytes handler with the entire virtual memory region as the argument.
      return Self.Dump_Memory_Region_Bytes (Self.Virtual_Region);
   end Dump_Memory_Region;

   -- Helper function which checks the virtual memory region to dump and make sure it is not out of range:
   function Is_Virtual_Memory_Region_Valid (Self : in out Instance; Arg : in Virtual_Memory_Region.T) return Boolean is
   begin
      if Arg.Length > 0 then -- A length of zero makes no sense, so reject it.
         declare
            Start_Address : constant Natural := Arg.Address;
            -- Length >= 1 and less than virtual region length so this will be positive
            End_Address : constant Unsigned_32 := Unsigned_32 (Arg.Address) + Unsigned_32 (Arg.Length) - 1;
            Region_Start_Address : constant Natural := Self.Virtual_Region.Address;
            Region_End_Address : constant Integer := Self.Virtual_Region.Address + Self.Virtual_Region.Length - 1;
         begin
            -- Make sure start address and end address fit inside the index region of our bytes array:
            if Start_Address >= Region_Start_Address and then
                End_Address <= Unsigned_32 (Region_End_Address) and then
                End_Address >= Unsigned_32 (Start_Address)
            then
               return True;
            end if;
         end;
      end if;

      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Memory_Region (Self.Sys_Time_T_Get, (Invalid_Region => Arg, Managed_Region => Self.Virtual_Region)));

      -- Requested virtual memory region does not fit in our managed region:
      return False;
   end Is_Virtual_Memory_Region_Valid;

   -- Helper which converts a Virtual_Memory_Region.T to a Virtual_Memory_Region_Positive.T and then checks to see if it is valid.
   function Is_Virtual_Memory_Region_Positive_Valid (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) return Boolean is
      Vmr : constant Virtual_Memory_Region.T := (Address => Arg.Address, Length => Arg.Length);
   begin
      return Self.Is_Virtual_Memory_Region_Valid (Vmr);
   end Is_Virtual_Memory_Region_Positive_Valid;

   -- Dump the memory region at the provided virtual address and length.
   overriding function Dump_Memory_Region_Bytes (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Is_Virtual_Memory_Region_Positive_Valid (Arg) then
         -- Throw event:
         Self.Event_T_Send_If_Connected (Self.Events.Dumping_Memory (Self.Sys_Time_T_Get, Arg));

         declare
            -- Create a pointer to the memory region we are managing
            Ptr : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Packed.Unpack (Self.Region);
            -- Slice the pointer with the virtual memory region to dump.
            Slice : constant Byte_Array_Pointer.Instance := Byte_Array_Pointer.Slice (Ptr, Start_Index => Arg.Address, End_Index => Arg.Address + Arg.Length - 1);
         begin
            -- Dump the memory:
            Self.Memory_Dump_Send_If_Connected ((Id => Self.Packets.Get_Memory_Region_Packet_Id, Memory_Pointer => Slice));
            return Success;
         end;
      end if;

      -- Region not valid:
      return Failure;
   end Dump_Memory_Region_Bytes;

   -- Perform a CRC on the region with the provided virtual address and length. The CRC will be reported via event and data product, if those connectors are connected.
   overriding function Crc_Memory_Region_Bytes (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Is_Virtual_Memory_Region_Positive_Valid (Arg) then
         -- Throw event:
         Self.Event_T_Send_If_Connected (Self.Events.Crcing_Memory (Self.Sys_Time_T_Get, Arg));

         declare
            -- Perform CRC on virtual memory region:
            Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Self.Bytes (Self.Bytes'First + Arg.Address .. Self.Bytes'First + Arg.Address + Arg.Length - 1));
            The_Time : Sys_Time.T;
         begin
            -- Grab time after performing CRC:
            The_Time := Self.Sys_Time_T_Get;
            -- Update data product:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Crc_Report (The_Time, (Region => (Address => Arg.Address, Length => Arg.Length), Crc => Crc)));
            -- Throw event:
            Self.Event_T_Send_If_Connected (Self.Events.Memory_Crc (The_Time, (Region => (Address => Arg.Address, Length => Arg.Length), Crc => Crc)));
            return Success;
         end;
      end if;

      -- Region not valid:
      return Failure;
   end Crc_Memory_Region_Bytes;

   -- Perform a write to the memory region at the provided address. If the memory is not available an error event will be produced.
   overriding function Write_Memory_Region (Self : in out Instance; Arg : in Virtual_Memory_Region_Write.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Region : constant Virtual_Memory_Region.T := (Address => Arg.Address, Length => Arg.Length);
   begin
      if Self.Is_Virtual_Memory_Region_Valid (Region) then
         -- Throw event:
         Self.Event_T_Send_If_Connected (Self.Events.Writing_Memory (Self.Sys_Time_T_Get, Region));

         -- Call our internal connector handler directly to request the memory region to us.
         declare
            use Memory_Manager_Enums.Memory_Request_Status;
            Request : constant Memory_Region_Request.T := Self.Memory_Region_Request_T_Return;
         begin
            -- Check the status:
            case Request.Status is
               -- If the request failed, then the command errored. The connector call produced an event.
               when Failure =>
                  return Failure;
               when Success =>
                  null; -- continue on
            end case;

            -- Write the memory:
            declare
               use Byte_Array_Pointer;
               use Byte_Array_Pointer.Packed;
               Ptr : constant Byte_Array_Pointer.Instance := Unpack (Request.Ided_Region.Region);
               Copy_To_Slice : constant Byte_Array_Pointer.Instance := Slice (Ptr, Start_Index => Region.Address, End_Index => Region.Address + Region.Length - 1);
            begin
               Copy_To (Copy_To_Slice, Arg.Data (Arg.Data'First .. Arg.Data'First + Region.Length - 1));
            end;

            -- Release the memory:
            Self.Ided_Memory_Region_T_Release (Request.Ided_Region);

            -- Throw info event and return success:
            Self.Event_T_Send_If_Connected (Self.Events.Memory_Written (Self.Sys_Time_T_Get, Region));
            return Success;
         end;
      end if;

      -- Region not valid:
      return Failure;
   end Write_Memory_Region;

   -- Forces the release of the memory region if it is currently allocated. This command can be used to recover from an anomalous condition.
   overriding function Force_Release (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Release the memory:
      Self.Arbiter.Force_Release (Self);
      -- Throw event
      Self.Event_T_Send_If_Connected (Self.Events.Memory_Force_Released (Self.Sys_Time_T_Get));
      return Success;
   end Force_Release;

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

end Component.Memory_Manager.Implementation;
