with Basic_Types;
with Configuration;

-- Declaration of parameter id type. This ensures that
-- it is NOT just a naked natural, giving the compiler
-- more information to help find errors.
package Parameter_Types is
   -- Id type:
   type Parameter_Id is new Natural range 0 .. 65_535;
   subtype Parameter_Id_Base is Parameter_Id;
   -- Length type:
   subtype Parameter_Buffer_Length_Type is Natural range 0 .. Configuration.Parameter_Buffer_Size;
   subtype Parameter_Buffer_Index_Type is Parameter_Buffer_Length_Type range 0 .. Parameter_Buffer_Length_Type'Last - 1;
   -- Buffer type:
   subtype Parameter_Buffer_Type is Basic_Types.Byte_Array (Parameter_Buffer_Index_Type);
   -- Parameter table entry ID:
   type Parameter_Table_Entry_Id is new Natural range 0 .. 65_535;
end Parameter_Types;
