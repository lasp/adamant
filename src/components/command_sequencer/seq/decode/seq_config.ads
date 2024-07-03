with Command_Types; use Command_Types;
with Data_Product_Types; use Data_Product_Types;
with Command;
with Ada.Containers.Indefinite_Ordered_Maps;
with Interfaces; use Interfaces;

package Seq_Config is

   --
   -- Types
   --

   -- Instance type:
   type Instance is tagged private;

   -- Seq types:
   type Seq_Param_Type is (Unsigned, Int, Discrete, Float);
   subtype Seq_String is String (1 .. 1024);

   -- Command parameter definitions:
   type Seq_Cmd_Param_Def is record
      Name : Seq_String := [others => ASCII.NUL];
      Offset : Natural := 0;
      Size : Positive := 1;
      Data_Type : Seq_Param_Type;
   end record;
   type Seq_Cmd_Param_Array is array (Natural range <>) of Seq_Cmd_Param_Def;
   type Seq_Cmd_Param_Array_Access is access all Seq_Cmd_Param_Array;

   -- Command definition:
   type Seq_Cmd_Def is record
      Name : Seq_String := [others => ASCII.NUL];
      Parameters : Seq_Cmd_Param_Array_Access := null;
      Command_Def : Command.T;
   end record;

   -- Telemetry definition:
   type Seq_Tlm_Def is record
      Name : Seq_String := [others => ASCII.NUL];
      Id : Data_Product_Types.Data_Product_Id;
   end record;

   --
   -- Primitives
   --

   procedure Init (Self : in out Instance; Path : in String);
   function Get_Command_Name (Self : in Instance; Id : in Command_Types.Command_Id) return String;
   function Get_Telemetry_Name (Self : in Instance; Id : in Data_Product_Types.Data_Product_Id; Offset : in Interfaces.Unsigned_16) return String;

private

   package Seq_Config_Cmd_Dict_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (
      Key_Type => Command_Types.Command_Id,
      Element_Type => Seq_Cmd_Def
   );

   package Seq_Config_Tlm_Dict_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (
      Key_Type => Interfaces.Unsigned_32,
      Element_Type => Seq_Tlm_Def
   );

   type Instance is tagged record
      Commands : Seq_Config_Cmd_Dict_Pkg.Map;
      Telemetry : Seq_Config_Tlm_Dict_Pkg.Map;
   end record;

end Seq_Config;
