with Seq_Types; use Seq_Types;
with Basic_Types; use Basic_Types;
with Memory_Region;
with Seq; use Seq;
with Command_Types; use Command_Types;
with Interfaces;

package Seq_Simulator is

   type Instance is tagged private;

   function Initialize (Self : in out Instance; Num_Engines : in Sequence_Engine_Id; Stack_Size : in Max_Seq_Num; Start_Source_Id : in Command_Source_Id) return Boolean;
   procedure Simulate (Self : in out Instance; Filepath : in String; To_Load : in Sequence_Engine_Id; Engine_Time_S : in Interfaces.Unsigned_32);

private
   type Seq_Engine_Array is array (Seq_Types.Sequence_Engine_Id range <>) of Seq.Engine;
   type Seq_Engine_Array_Access is access all Seq_Engine_Array;

   function Load_Sequence_In_Memory (Path : in String; Buffer : in Basic_Types.Byte_Array_Access; Sequence : out Memory_Region.T) return Boolean;

   type Instance is tagged record
      Seq_Engines : Seq_Engine_Array_Access := null;
   end record;

end Seq_Simulator;
