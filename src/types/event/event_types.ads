with Basic_Types;
with Configuration;

-- Declaration of event id type. This ensures that
-- it is NOT just a naked natural, giving the compiler
-- more information to help find errors.
package Event_Types is
   -- Id type:
   type Event_Id is new Natural range 0 .. 65_535;
   subtype Event_Id_Base is Event_Id range 1 .. Event_Id'Last;
   -- Length type:
   subtype Parameter_Buffer_Length_Type is Natural range 0 .. Configuration.Event_Buffer_Size;
   subtype Parameter_Buffer_Index_Type is Parameter_Buffer_Length_Type range 0 .. Parameter_Buffer_Length_Type'Last - 1;
   -- Buffer type:
   subtype Parameter_Buffer_Type is Basic_Types.Byte_Array (Parameter_Buffer_Index_Type);
end Event_Types;
