with Basic_Types;
with Configuration;

-- Declaration of command id type. This ensures that
-- it is NOT just a naked natural, giving the compiler
-- more information to help find errors.
package Command_Types is
   -- Id type:
   type Command_Id is new Natural range 0 .. 65_535;
   subtype Command_Id_Base is Command_Id range 1 .. Command_Id'Last;
   type Command_Registration_Id is new Natural range 0 .. 65_535;
   type Command_Source_Id is new Natural range 0 .. 65_535;
   -- Length type:
   subtype Command_Arg_Buffer_Length_Type is Natural range 0 .. Configuration.Command_Buffer_Size;
   subtype Command_Arg_Buffer_Index_Type is Command_Arg_Buffer_Length_Type range 0 .. Command_Arg_Buffer_Length_Type'Last - 1;
   -- Buffer type:
   subtype Command_Arg_Buffer_Type is Basic_Types.Byte_Array (Command_Arg_Buffer_Index_Type);
end Command_Types;
