with Basic_Types;
with Configuration;

-- Declaration of fault id type. This ensures that
-- it is NOT just a naked natural, giving the compiler
-- more information to help find errors.
package Fault_Types is
   -- Id type:
   type Fault_Id is new Natural range 0 .. 65_535;
   subtype Fault_Id_Base is Fault_Id range 1 .. Fault_Id'Last;
   -- Length type:
   subtype Parameter_Buffer_Length_Type is Natural range 0 .. Configuration.Fault_Buffer_Size;
   subtype Parameter_Buffer_Index_Type is Parameter_Buffer_Length_Type range 0 .. Parameter_Buffer_Length_Type'Last - 1;
   -- Buffer type:
   subtype Parameter_Buffer_Type is Basic_Types.Byte_Array (Parameter_Buffer_Index_Type);
end Fault_Types;
