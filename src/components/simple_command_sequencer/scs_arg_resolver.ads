-- scs_arg_resolver.ads
with Command_Types;
with Basic_Types;

package Scs_Arg_Resolver is
   type T is abstract tagged null record;
   type T_Access is access constant T'Class;

   -- Resolve a dynamic command argument from a sequence's per-call argument
   -- buffer. The buffer is passed as a typed Byte_Array (not an Address), so a
   -- concrete resolver can only deserialize the buffer through the input type's
   -- safe Serialization.From_Byte_Array; it cannot reinterpret arbitrary memory
   -- as an arbitrary record.
   function Resolve (
      Resolver : T;
      Data     : Basic_Types.Byte_Array
   ) return Command_Types.Command_Arg_Buffer_Type is abstract;
end Scs_Arg_Resolver;
