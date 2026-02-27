--------------------------------------------------------------------------------
-- Sequence_Store Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Sequence_Store_Memory_Region_Store;
with Sequence_Store_Memory_Region_Fetch;
with Packed_Sequence_Id;
with Binary_Tree;
with Sequence_Store_Types;
with Sequence_Types;

-- The Sequence Store component is responsible for storing and managing access to a set of memory regions (slots) which each hold a single sequence. The managed memory regions are usually located in nonvolatile storage and can be read or written to via this component.
package Component.Sequence_Store.Implementation is

   -- The component class instance record:
   type Instance is new Sequence_Store.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory regions (slots) it is to manage.
   --
   -- Init Parameters:
   -- Sequence_Slots : Sequence_Slot_Array_Access - A list of memory regions. Each represents a slot to hold a single sequence of equal or smaller size (including header). This list will be copied into the component at initialization onto a heap allocated memory. The memory regions must not overlap in any way, must be large enough to at least hold a sequence header, and the list must not be empty. These properties will be enforced by the component via assertions when Init is called.
   -- Check_Slots_At_Startup : Boolean - If True, then check the validity of the sequences in all slots by computing CRCs over them at startup.
   -- Dump_Slot_Summary_At_Startup : Boolean - If True, then the slot summaries will be dumped at startup.
   --
   overriding procedure Init (Self : in out Instance; Sequence_Slots : in not null Sequence_Slot_Array_Access; Check_Slots_At_Startup : in Boolean; Dump_Slot_Summary_At_Startup : in Boolean);
   not overriding procedure Final (Self : in out Instance);

private

   -- Elements of this element type will be stored in a binary tree to lookup
   -- the slot location of a sequence.
   type Sequence_Lookup_Element is record
      Id : Sequence_Types.Sequence_Id := Sequence_Types.Sequence_Id'First;
      Slot : Sequence_Store_Types.Slot_Number := Sequence_Store_Types.Slot_Number'First;
   end record;

   -- Define the binary tree package which enables us
   -- to lookup the sequence by ID in O(log n) time.
   use Sequence_Types;
   function Less_Than (Left, Right : in Sequence_Lookup_Element) return Boolean is (Left.Id < Right.Id) with
      Inline => True;
   function Greater_Than (Left, Right : in Sequence_Lookup_Element) return Boolean is (Left.Id > Right.Id) with
      Inline => True;
   package Sequence_Lookup_B_Tree is new Binary_Tree (Sequence_Lookup_Element, Less_Than, Greater_Than);

   -- Status types for binary tree protected object below:
   type Add_Status is (Success, Duplicate_Id);
   type Find_Status is (Success, Id_Not_Found);

   -- Define a protected object to protect access to the binary tree.
   -- We do this because fetching a sequence can happen at the same time as
   -- adding a sequence, and we need to provide mutual exclusion to the
   -- data structure.
   protected type Protected_Sequence_Lookup_B_Tree is
      -- Procedures requiring full mutual exclusion:
      procedure Init (Maximum_Size : in Positive);
      procedure Destroy;
      -- Add a sequence to the tree if a sequence of that ID doesn't already exist in the tree.
      procedure Add_Sequence (Id : in Sequence_Types.Sequence_Id; Slot : in Sequence_Store_Types.Slot_Number; Status : out Add_Status);
      -- Remove a sequence from the tree if it exists in the tree.
      procedure Remove_Sequence (Id : in Sequence_Types.Sequence_Id; Status : out Find_Status);
      -- Find a sequence in the tree if it exists.
      function Find_Sequence (Id : in Sequence_Types.Sequence_Id; Slot : out Sequence_Store_Types.Slot_Number) return Find_Status;
   private
      -- The binary tree:
      Tree : Sequence_Lookup_B_Tree.Instance;
   end Protected_Sequence_Lookup_B_Tree;

   -- The component class instance record:
   type Instance is new Sequence_Store.Base_Instance with record
      -- An array of memory regions, each of which holds a sequence.
      Slots : Sequence_Slot_Array_Access := null;
      -- A list (binary tree) of active sequences. This allows for
      -- quick lookup of available sequences without having to linearly
      -- look through all the slots.
      Active_Sequence_List : Protected_Sequence_Lookup_B_Tree;
      -- Boolean which determines whether or not to dump the slot summary at startup.
      Dump_Slot_Summary_At_Startup : Boolean := False;
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
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- This connector is used to load a new sequence into a slot in the store.
   overriding procedure Sequence_Store_Memory_Region_Store_T_Recv_Async (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T);
   -- This procedure is called when a Sequence_Store_Memory_Region_Store_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Sequence_Store_Memory_Region_Store_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T);
   -- This connector is used to fetch a pointer to a sequence found in the store given its ID.
   overriding function Sequence_Store_Memory_Region_Fetch_T_Service (Self : in out Instance; Arg : in Packed_Sequence_Id.T) return Sequence_Store_Memory_Region_Fetch.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Sequence_Store_Memory_Region_Release_T_Send message is dropped due to a full queue.
   overriding procedure Sequence_Store_Memory_Region_Release_T_Send_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Release.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Parameter Store component.
   -- Activate a sequence slot so that its contents can be fetched.
   overriding function Activate_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) return Command_Execution_Status.E;
   -- Deactivate a sequence slot so that its contents can no longer be fetched.
   overriding function Deactivate_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) return Command_Execution_Status.E;
   -- Check the CRC of a sequence in a particular slot to see if it matches the CRC found in the header.
   overriding function Check_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) return Command_Execution_Status.E;
   -- Check the CRC of sequences in all sequence store slots to see if they match the CRCs found in their headers.
   overriding function Check_All_Slots (Self : in out Instance) return Command_Execution_Status.E;
   -- Produce a packet with the current storage slot summary information.
   overriding function Dump_Summary (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Sequence_Store.Implementation;
