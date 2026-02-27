--------------------------------------------------------------------------------
-- Logger Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Command;

-- Includes:
with Circular_Buffer;
with Circular_Buffer_Meta;
with Logger_Enums; use Logger_Enums;

-- The Logger component receives data of generic type. This data is synchronously added to an internal circular buffer. By default, the logging of this data is disabled at component start and can be enabled via command. Commands also exist to dump the internal circular buffer.
generic
package Component.Logger.Implementation is

   -- The component class instance record:
   type Instance is new Logger.Base_Instance with private;

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
   overriding procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access := null; Meta_Data : in Circular_Buffer_Meta.T_Access := null; Size : in Integer := -1; Initial_Mode : in Logger_Enums.Logger_Mode.E := Logger_Enums.Logger_Mode.Disabled);
   not overriding procedure Final (Self : in out Instance);

private

   -- Protected buffer type to provide mutual exclusion of internal buffer:
   protected type Protected_Buffer is
      -- Functions that provide read-only access to the private data:
      function Get_Meta_Data return Circular_Buffer_Meta.T;
      function Get_Mode return Logger_Mode.E;
      function Dump return Circular_Buffer.Pointer_Dump;
      function Dump_Newest (Num_Bytes_To_Dump : in Natural) return Circular_Buffer.Pointer_Dump;
      function Dump_Oldest (Num_Bytes_To_Dump : in Natural) return Circular_Buffer.Pointer_Dump;
      function Dump_Memory return Circular_Buffer.Pointer_Dump;
      -- Procedures requiring full mutual exclusion:
      procedure Init (Bytes : in Basic_Types.Byte_Array_Access; Meta_Data_Access : in Circular_Buffer_Meta.T_Access);
      procedure Destroy;
      procedure Clear;
      procedure Push (Src : in T; Num_Bytes_To_Store : out Natural; Status : out Log_Attempt_Status.E);
      procedure Set_Mode (New_Mode : in Logger_Mode.E);
      procedure Save_Meta_Data;
   private
      Buffer : Circular_Buffer.Circular;
      Meta_Data : Circular_Buffer_Meta.T_Access;
      Mode : Logger_Mode.E := Logger_Mode.Disabled;
   end Protected_Buffer;

   -- The component class instance record:
   type Instance is new Logger.Base_Instance with record
      -- The internal circular buffer:
      Buffer : Protected_Buffer;
      -- The allocation of bytes to the circular buffer:
      Bytes : Basic_Types.Byte_Array_Access := null;
      -- A pointer to the meta data:
      Meta_Data : Circular_Buffer_Meta.T_Access := null;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic log data connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Memory_Dump_Send message is dropped due to a full queue.
   overriding procedure Memory_Dump_Send_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the logger component.
   -- Enable the logger to start saving data.
   overriding function Enable (Self : in out Instance) return Command_Execution_Status.E;
   -- Disable the logger from saving received data.
   overriding function Disable (Self : in out Instance) return Command_Execution_Status.E;
   -- Dump the entire log oldest to newest data.
   overriding function Dump_Log (Self : in out Instance) return Command_Execution_Status.E;
   -- Dump the newest X bytes of data from the log.
   overriding function Dump_Newest_Data (Self : in out Instance; Arg : in Packed_Positive_Length.T) return Command_Execution_Status.E;
   -- Dump the oldest X bytes of data from the log.
   overriding function Dump_Oldest_Data (Self : in out Instance; Arg : in Packed_Positive_Length.T) return Command_Execution_Status.E;
   -- Dump the entire region of memory associated with the logger from start to finish in memory byte order.
   overriding function Dump_Log_Memory (Self : in out Instance) return Command_Execution_Status.E;
   -- Send an event out with the meta data of the log.
   overriding function Send_Meta_Data_Event (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Logger.Implementation;
