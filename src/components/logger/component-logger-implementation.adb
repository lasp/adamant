--------------------------------------------------------------------------------
-- Logger Component Implementation Body
--------------------------------------------------------------------------------

with Serializer_Types;
with Byte_Array_Pointer.Packed;
with Sys_Time;

package body Component.Logger.Implementation is

   --------------------------------------------------
   -- Subprogram for the internal protected object:
   --------------------------------------------------

   protected body Protected_Buffer is

      function Get_Meta_Data return Circular_Buffer_Meta.T is
      begin
         return Meta_Data.all;
      end Get_Meta_Data;

      function Get_Mode return Logger_Mode.E is
      begin
         return Mode;
      end Get_Mode;

      function Dump return Circular_Buffer.Pointer_Dump is
      begin
         return Buffer.Dump;
      end Dump;

      function Dump_Newest (Num_Bytes_To_Dump : in Natural) return Circular_Buffer.Pointer_Dump is
      begin
         return Buffer.Dump_Newest (Num_Bytes_To_Dump);
      end Dump_Newest;

      function Dump_Oldest (Num_Bytes_To_Dump : in Natural) return Circular_Buffer.Pointer_Dump is
      begin
         return Buffer.Dump_Oldest (Num_Bytes_To_Dump);
      end Dump_Oldest;

      function Dump_Memory return Circular_Buffer.Pointer_Dump is
      begin
         return Buffer.Dump_Memory;
      end Dump_Memory;

      procedure Init (Bytes : in Basic_Types.Byte_Array_Access; Meta_Data_Access : in Circular_Buffer_Meta.T_Access) is
      begin
         Buffer.Init (Bytes);
         Meta_Data := Meta_Data_Access;
      end Init;

      procedure Destroy is
      begin
         Buffer.Destroy;
      end Destroy;

      procedure Clear is
      begin
         Buffer.Clear;
      end Clear;

      procedure Push (Src : in T; Num_Bytes_To_Store : out Natural; Status : out Log_Attempt_Status.E) is
         use Serializer_Types;
         use Logger_Enums.Log_Attempt_Status;
         -- Get the serialized length of the source:
         Stat : constant Serialization_Status := Serialized_Length (Src, Num_Bytes_To_Store);
      begin
         -- Make sure source has a valid length:
         if Stat /= Success then
            Status := Serialization_Failure;
         else
            -- Push the data onto the circular buffer:
            declare
               use Circular_Buffer;
               -- Overlay source type with properly sized byte array:
               subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_To_Store - 1);
               subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
               pragma Warnings (Off, "overlay changes scalar storage order");
               Bytes : constant Sized_Byte_Array with
                  Import,
                  Convention => Ada,
                  Address => Src'Address;
               pragma Warnings (On, "overlay changes scalar storage order");
            begin
               case Buffer.Push (Bytes, Overwrite => True) is
                  when Success =>
                     Status := Success;
                  when Too_Full =>
                     Status := Too_Full;
               end case;
            end;

            -- Save off the meta data:
            Meta_Data.all := Buffer.Get_Meta_Data;
         end if;
      end Push;

      procedure Set_Mode (New_Mode : in Logger_Mode.E) is
      begin
         Mode := New_Mode;
      end Set_Mode;

      procedure Save_Meta_Data is
      begin
         Meta_Data.all := Buffer.Get_Meta_Data;
      end Save_Meta_Data;

   end Protected_Buffer;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This init function provides memory allocation for the logger's internal memory. Preallocated memory can be provided via the "bytes" access type, in which case "size" must be negative and will be ignored. If you would like to allocate the internal memory on the heap then "bytes" must be set to null, and "size" must be a positive number representing the number of bytes you would like to allocate.
   --
   -- Init Parameters:
   -- bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to be used for storing log data. If this is set to null, then memory will be allocated on the heap using the "size" parameter instead. Note: This must be set to null if the "size" parameter is positive below.
   -- Meta_Data : Circular_Buffer_Meta.T_Access - A pointer to an allocation of a meta data record for storing the log meta data. This can be used to place the meta data where desired in memory. This item must be set to null if "size" is positive, and non-null if "bytes" is non-null.
   -- size : Integer - The number of bytes to allocate on the heap for memory storage. Note: This must be set to a negative value if the "bytes" parameters is not null.
   -- Initial_Mode : Logger_Enums.Logger_Mode.E - The initial mode of the logger (enabled/disabled) upon initialization
   --
   overriding procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access := null; Meta_Data : in Circular_Buffer_Meta.T_Access := null; Size : in Integer := -1; Initial_Mode : in Logger_Enums.Logger_Mode.E := Logger_Enums.Logger_Mode.Disabled)
   is
      use Basic_Types;
      use Circular_Buffer_Meta;
   begin
      -- If bytes is null make sure size is positive and Meta_Data is also null:
      if Bytes = null then
         pragma Assert (Size > 0, "Logger Init Error: If a null 'bytes' pointer is provided, then a positive size must be provided to allocate memory on the heap.");
         pragma Assert (Meta_Data = null, "Logger Init Error: If a null 'bytes' pointer is provided, then a null meta_Data pointer must be provided.");
      else
         pragma Assert (Meta_Data /= null, "Logger Init Error: If a non-null 'bytes' pointer is provided, then a non-null meta_Data pointer must be provided.");
      end if;

      -- If bytes is negative make sure bytes is valid:
      if Size <= 0 then
         pragma Assert (Bytes /= null, "Logger Init Error: If size is negative or zero then a valid 'bytes' access type must be provided.");
         pragma Assert (Meta_Data /= null, "Logger Init Error: If size is negative or zero then a valid 'meta_Data' access type must be provided.");
      end if;

      -- Make sure only bytes or size is provided, not both:
      if Size > 0 and then (Bytes /= null or else Meta_Data /= null) then
         pragma Assert (False, "Logger Init Error: Either size > 0 or bytes /= null should be provided to size the logger's internal buffer, not both!");
      end if;
      pragma Annotate (GNATSAS, Intentional, "condition predetermined", "Defensive check - and also good documentation event if redundant with previous assertions");

      -- Initialize the internal byte buffer:
      if Bytes /= null then
         Self.Bytes := Bytes;
         pragma Assert (Meta_Data /= null, "This should never happen.");
         Self.Meta_Data := Meta_Data;
         -- Initialize circular buffer with bytes following the meta data storage:
         Self.Buffer.Init (Self.Bytes, Self.Meta_Data);
      else
         Self.Bytes := new Basic_Types.Byte_Array (0 .. Size - 1);
         Self.Bytes.all := [others => 0]; -- Initialize to zeros if this is on the heap.
         pragma Assert (Meta_Data = null, "This should never happen.");
         Self.Meta_Data := new Circular_Buffer_Meta.T;
         -- Initialize circular buffer with bytes following the meta data storage:
         Self.Buffer.Init (Self.Bytes, Self.Meta_Data);
         -- Save off the initialized meta data. This only makes sense to do if we
         -- allocated the meta data on the heap, then the heap data is invalid anyways, so
         -- let's overwrite it with properly initialized meta data:
         Self.Buffer.Save_Meta_Data;
      end if;

      -- Set the default logger state:
      Self.Buffer.Set_Mode (Initial_Mode);
   end Init;

   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update the data products:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Mode (Self.Sys_Time_T_Get, (Current_Mode => Self.Buffer.Get_Mode)));
   end Set_Up;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Buffer.Destroy;
      Self.Buffer.Set_Mode (Logger_Mode.Disabled);
   end Final;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic log data connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
      use Logger_Enums.Logger_Mode;
      use Logger_Enums.Log_Attempt_Status;

      Status : Log_Attempt_Status.E;
      Num_Bytes_Stored : Natural;
   begin
      -- If we are enabled, then push data onto the buffer:
      case Self.Buffer.Get_Mode is
         when Enabled =>
            -- Push the received type onto the internal circular buffer:
            Self.Buffer.Push (Arg, Num_Bytes_Stored, Status);

            -- If the status is bad, then send out an event:
            if Status /= Success then
               Self.Event_T_Send_If_Connected (Self.Events.Log_Attempt_Failed (Self.Sys_Time_T_Get, (Num_Bytes_Logged => Num_Bytes_Stored, Status => Status)));
            end if;
         when Disabled =>
            null;
      end case;
   end T_Recv_Sync;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the logger component.

   -- Enable the logger to start saving data.
   overriding function Enable (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new mode:
      Self.Buffer.Set_Mode (Logger_Mode.Enabled);
      -- Send out event:
      Self.Event_T_Send_If_Connected (Self.Events.Log_Enabled (Time));
      -- Update the data products:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Mode (Time, (Current_Mode => Logger_Mode.Enabled)));
      return Success;
   end Enable;

   -- Disable the logger from saving received data.
   overriding function Disable (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new mode:
      Self.Buffer.Set_Mode (Logger_Mode.Disabled);
      -- Send out event:
      Self.Event_T_Send_If_Connected (Self.Events.Log_Disabled (Time, Self.Buffer.Get_Meta_Data));
      -- Update the data products:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Mode (Time, (Current_Mode => Logger_Mode.Disabled)));
      return Success;
   end Disable;

   -- Helper function for dumping a data pointer:
   procedure Dump_Ptr (Self : in out Instance; Ptr : in Byte_Array_Pointer.Instance) is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;
   begin
      if (not Is_Null (Ptr)) and then Length (Ptr) > 0 then
         -- Do the dump:
         Self.Memory_Dump_Send ((Id => Self.Packets.Get_Log_Packet_Id, Memory_Pointer => Ptr));
         -- Send event:
         Self.Event_T_Send_If_Connected (Self.Events.Dumping_Log_Memory (Self.Sys_Time_T_Get, Pack (Ptr)));
      end if;
   end Dump_Ptr;

   -- Helper function for dumping the log meta data:
   procedure Dump_Meta (Self : in out Instance) is
      function Ptr_From_Meta is new Byte_Array_Pointer.From_Typed_Access (Circular_Buffer_Meta.T, Circular_Buffer_Meta.T_Access);
      Ptr : constant Byte_Array_Pointer.Instance := Ptr_From_Meta (Self.Meta_Data);
   begin
      -- Do the dump:
      Dump_Ptr (Self, Ptr);
   end Dump_Meta;

   function Do_Dump (Self : in out Instance; Dump : in Circular_Buffer.Pointer_Dump) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- First dump the meta data:
      Dump_Meta (Self);

      -- Send out the pointers:
      for Ptr of Dump loop
         Dump_Ptr (Self, Ptr);
      end loop;
      return Success;
   end Do_Dump;

   -- Dump the entire log oldest to newest data.
   overriding function Dump_Log (Self : in out Instance) return Command_Execution_Status.E is
   begin
      -- Dump log dump:
      return Do_Dump (Self, Self.Buffer.Dump);
   end Dump_Log;

   -- Dump the newest X bytes of data from the log.
   overriding function Dump_Newest_Data (Self : in out Instance; Arg : in Packed_Positive_Length.T) return Command_Execution_Status.E is
   begin
      -- Get the log dump:
      return Do_Dump (Self, Self.Buffer.Dump_Newest (Arg.Length));
   end Dump_Newest_Data;

   -- Dump the oldest X bytes of data from the log.
   overriding function Dump_Oldest_Data (Self : in out Instance; Arg : in Packed_Positive_Length.T) return Command_Execution_Status.E is
   begin
      -- Get the log dump:
      return Do_Dump (Self, Self.Buffer.Dump_Oldest (Arg.Length));
   end Dump_Oldest_Data;

   -- Dump the entire region of memory associated with the logger from start to finish in memory byte order.
   overriding function Dump_Log_Memory (Self : in out Instance) return Command_Execution_Status.E is
   begin
      -- Get the log dump:
      return Do_Dump (Self, Self.Buffer.Dump_Memory);
   end Dump_Log_Memory;

   -- Send an event out with the meta data of the log.
   overriding function Send_Meta_Data_Event (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Log_Info_Update (Self.Sys_Time_T_Get, (Meta_Data => Self.Buffer.Get_Meta_Data, Current_Mode => Self.Buffer.Get_Mode)));
      return Success;
   end Send_Meta_Data_Event;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

end Component.Logger.Implementation;
