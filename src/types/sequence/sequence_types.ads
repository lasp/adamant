-- Declaration of Sequence id type. This ensures that
-- it is NOT just a naked natural, giving the compiler
-- more information to help find errors.
package Sequence_Types is
   -- Id type:
   type Sequence_Id is new Natural range 0 .. 65_535;
   subtype Sequence_Length_Type is Natural range 0 .. 65_535;
end Sequence_Types;
