with Basic_Types;
with Packed_Poly_32_Type;

package Seq_Types is

   --
   -- SEQ Mission Configuration Parameter Definitions
   --
   -- The SEQ compiler defines the following "Mission Parameters"
   -- which are defined here in the Adamant system. If these variables
   -- are assigned elsewhere a note is provided.
   --
   -- SEQ_NUM_ENGINES 16
   -- Defined at command sequencer component initialization - Num_Engines.
   --
   -- SEQ_MAX_BUFFER_SIZE 16384
   -- Defined in sequence store initialization, or similar component that
   -- stores sequences. The command sequencer itself can handle sequences up to
   -- 65535 bytes in length.
   Max_Seq_Size : constant Natural := 2**16;
   --
   -- SEQ_MAX_CMD_SIZE 1024
   -- Defined in Adamant mission configuration file, see User Guide.
   --
   -- SEQ_NUM_LOCAL_VARIABLES 16
   -- Constants for the Adamant implementation of the command sequencer:
   Num_Seq_Variables : constant Natural := 16;
   --
   -- SEQ_NUM_GLOBAL_VARIABLES 48
   -- Adamant does not support global variables, so this is implicitly zero.
   --
   -- SEQ_MAX_NESTED 3
   -- Defined at command sequencer component initialization - Stack_Size.
   --
   -- SEQ_MAX_NAME_SIZE 20
   -- Sequences are tracked in Adamant by 16-bit ID. There are no string names included
   -- in compiled sequences.
   --
   -- SEQ_MAX_CMDS_PER_CYCLE 1
   -- Does not apply to Adamant implementation.
   --
   -- SEQ_MAX_TLM_WAIT_TIME 10
   -- Defined at command sequencer component initialization - Timeout_Limit. This only
   -- really applies to telemetry items that have not yet been supplied to the product
   -- database, thus we would be waiting for it to populate before checking. This is also
   -- used to timeout waiting on sequence loads and command responses.
   --
   -- SEQ_MAX_CATEGORIES 16
   -- Sequence categories are not supported by Adamant.
   --
   -- SEQ_MAX_ERROR_STRING_LEN 64
   -- Define the maximum length string allowed in events.
   Max_Seq_String_Size : constant Natural := 64;
   --
   -- SEQ_MAX_INSTR_LOOP 64
   -- Defined at command sequencer component initialization - Instruction_Limit
   --
   -- SEQ_FP_TOLERANCE 0.0001
   -- Adamant does not use an implicit floating point tolerance when comparing floats. So
   -- this could be thought of as hard coded to zero. Adamant requires the user implement
   -- their desired tolerance in the sequence itself when doing comparisons.
   --
   -- SEQ_MIN_LARGE_WAIT_THRESHOLD 345600
   -- Does not apply to Adamant implementation.
   --
   -- SEQ_SUBSCRIPTION_LIST_SIZE 2048
   -- Does not apply to Adamant implementation.
   --
   -- SEQ_SIZE_STRPOOL_STRINGS 64
   -- Does not apply to Adamant implementation.
   --

   --
   -- SEQ type definitions:
   --

   -- Type used to index into a sequence buffer.
   type Seq_Position is range 0 .. Max_Seq_Size - 1;

   -- A status enum for anytime something could fail
   type Seq_Status is (Success, Failure);

   -- Seq has two internal variables
   type Seq_Num_Internals is range 0 .. 3;
   type Internal_Array is array (Seq_Num_Internals) of Packed_Poly_32_Type.T;

   -- Declare the seq string type which is 64 bytes in length:
   subtype Seq_String is Basic_Types.Byte_Array (0 .. Max_Seq_String_Size - 1);

   -- A 2D array of byte_arrays. Seq can handle 16 local variables. Every local variable is stored as a byte_array of length 4, except for strings which are at max 64 bytes in length.
   -- Variable type is only determined when some sort of action on a variable occurs.
   type Seq_Local_Id is range 0 .. Num_Seq_Variables - 1;
   type Variable_Array is array (Seq_Local_Id) of Packed_Poly_32_Type.T;
   -- String pools not yet supported by Adamant implementation.
   -- type String_Pool is array (Seq_Local_Id) of Seq_String;

   -- Sequence engine ID type:
   type Sequence_Engine_Id is range 0 .. Basic_Types.Byte'Last;
   subtype Num_Engines_Type is Sequence_Engine_Id range 1 .. Sequence_Engine_Id'Last;

   -- The range of number of sequences an engine can hold (essentially engine stack depth)
   type Max_Seq_Num is range 0 .. Basic_Types.Byte'Last;
   subtype Stack_Depth_Type is Max_Seq_Num range 1 .. Max_Seq_Num'Last;
end Seq_Types;
