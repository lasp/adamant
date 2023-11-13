--------------------------------------------------------------------------------
-- Seq Tests Spec
--------------------------------------------------------------------------------

with Memory_Region;
with Basic_Types;
with Ada.Unchecked_Deallocation;
with Smart_Assert;
with Command_Types; use Command_Types;
with Seq; use Seq;
with Seq_Runtime;
with Seq_Types; use Seq_Types;
--with Seq_Internal_Code; use Seq_Internal_Code;
--with Seq_Wait_Type; use Seq_Wait_Type;
--with Seq_Eval_Ops; use Seq_Eval_Ops;

with Seq_Enums; use Seq_Enums;
with Sequence_Types;

-- Unit tests for seq_engine.
package Seq_Tests.Implementation is
   -- Test data and state:
   type Instance is new Seq_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private

   ------------------------------
   -- Custom Assertions
   ------------------------------
   package State_Assert is new Smart_Assert.Discrete (Seq_Runtime_State.E, Seq_Runtime_State.E'Image);
   package Exec_State_Assert is new Smart_Assert.Discrete (Seq_Execute_State.E, Seq_Execute_State.E'Image);
   package Seq_Error_Code_Assert is new Smart_Assert.Discrete (Seq_Error.E, Seq_Error.E'Image);
   package Engine_State_Assert is new Smart_Assert.Discrete (Seq_Engine_State.E, Seq_Engine_State.E'Image);
   package Command_Id_Assert is new Smart_Assert.Discrete (Command_Id, Command_Id'Image);
   package Command_Sid_Assert is new Smart_Assert.Discrete (Command_Source_Id, Command_Source_Id'Image);
   package Command_Arg_Buffer_Length_Type_Assert is new Smart_Assert.Discrete (Command_Arg_Buffer_Length_Type, Command_Arg_Buffer_Length_Type'Image);
   package Seq_Position_Assert is new Smart_Assert.Discrete (Seq_Position, Seq_Position'Image);
   package Seq_Positive_16_Assert is new Smart_Assert.Discrete (Basic_Types.Positive_16, Basic_Types.Positive_16'Image);
   package Internal_Src_Assert is new Smart_Assert.Discrete (Seq_Internal.E, Seq_Internal.E'Image);
   package Seq_Id_Assert is new Smart_Assert.Discrete (Sequence_Types.Sequence_Id, Sequence_Types.Sequence_Id'Image);
   package Seq_Engine_Id_Assert is new Smart_Assert.Discrete (Sequence_Engine_Id, Sequence_Engine_Id'Image);
   package Load_Status_Assert is new Smart_Assert.Discrete (Seq.Load_Status, Seq.Load_Status'Image);
   package Done_Waiting_Status_Assert is new Smart_Assert.Discrete (Seq.Done_Waiting_Status, Seq.Done_Waiting_Status'Image);

   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- A test on loading sequences into an engine.
   overriding procedure Load_Sequence_Test (Self : in out Instance);
   -- A test that runs valid sequences, checks for all sorts of behavior.
   overriding procedure Run_Valid_Sequences (Self : in out Instance);
   -- A test that a sequence that executes too many instructions is terminated.
   overriding procedure Instruction_Overflow (Self : in out Instance);
   -- A test for the set bit pattern instruction
   overriding procedure Set_Bit_Pattern (Self : in out Instance);
   -- A test for the send bit pattern instruction
   overriding procedure Send_Bit_Pattern (Self : in out Instance);
   -- A test for the update bit pattern instruction
   overriding procedure Update_Bit_Pattern (Self : in out Instance);
   -- A test for the call instruction
   overriding procedure Call (Self : in out Instance);
   -- A test for the spawn instruction
   overriding procedure Spawn (Self : in out Instance);
   -- A test for the start instruction
   overriding procedure Start (Self : in out Instance);
   -- A test for the Push instruction
   overriding procedure Push (Self : in out Instance);
   -- A test for the Eval instruction
   overriding procedure Eval (Self : in out Instance);
   -- A test for the Fetch Var instruction
   overriding procedure Fetch_Var (Self : in out Instance);
   -- A test for the Store_Var instruction
   overriding procedure Store_Var (Self : in out Instance);
   -- A test for the Fetch Tlm instruction
   overriding procedure Fetch_Tlm (Self : in out Instance);
   -- A test for the Wait instruction
   overriding procedure Wait (Self : in out Instance);
   -- A test for the Goto instruction
   overriding procedure Seq_Goto (Self : in out Instance);
   -- A test for the Jump if zero instruction
   overriding procedure Jump_If_Zero (Self : in out Instance);
   -- A test for the Jump not zero instruction
   overriding procedure Jump_Not_Zero (Self : in out Instance);
   -- A test for the Jump if equal instruction
   overriding procedure Jump_If_Equal (Self : in out Instance);
   -- A test for the Jump not equal instruction
   overriding procedure Jump_Not_Equal (Self : in out Instance);
   -- A test for the return instruction
   overriding procedure Seq_Return (Self : in out Instance);
   -- A test for the Wait if zero instruction
   overriding procedure Wait_If_Zero (Self : in out Instance);
   -- A test for the Eval float instruction
   overriding procedure Eval_Flt (Self : in out Instance);
   -- A test for the Cast F to U instruction
   overriding procedure Cast_F_To_U (Self : in out Instance);
   -- A test for the Cast U to F instruction
   overriding procedure Cast_U_To_F (Self : in out Instance);
   -- A test for the Eval S instruction
   overriding procedure Eval_S (Self : in out Instance);
   -- A test for the Cast S to U instruction
   overriding procedure Cast_S_To_U (Self : in out Instance);
   -- A test for the Cast U to S instruction
   overriding procedure Cast_U_To_S (Self : in out Instance);
   -- A test for the Cast F to S instruction
   overriding procedure Cast_F_To_S (Self : in out Instance);
   -- A test for the Cast S to F instruction
   overriding procedure Cast_S_To_F (Self : in out Instance);
   -- A test for the Wait on B instruction
   overriding procedure Wait_On_B (Self : in out Instance);
   -- A test for the Wait if zero on B instruction
   overriding procedure Wait_If_Zero_On_B (Self : in out Instance);
   -- A test for the Cmd Print instruction
   overriding procedure Cmd_Print (Self : in out Instance);
   -- A test for the Cmd Print Var instruction
   overriding procedure Cmd_Print_Var (Self : in out Instance);
   -- A test for the Cmd Str Alloc instruction. This is essentially a noop for Adamant, and is tested as such.
   overriding procedure Cmd_Str_Alloc (Self : in out Instance);
   -- A test for parsing a bad instruction with invalid field or opcode.
   overriding procedure Bad_Instruction (Self : in out Instance);

   ------------------------------
   -- Helper functions
   ------------------------------
   function Load_Sequence_In_Memory (File_Path : in String; Buffer : in Basic_Types.Byte_Array_Access) return Memory_Region.T;

   -- Deallocation for temporary buffer
   procedure Free is new Ada.Unchecked_Deallocation (Object => Basic_Types.Byte_Array, Name => Basic_Types.Byte_Array_Access);

   -- Test data and state:
   type Instance is new Seq_Tests.Base_Instance with record
      Wb_Runner : Seq_Runtime.Instance;
   end record;
end Seq_Tests.Implementation;
