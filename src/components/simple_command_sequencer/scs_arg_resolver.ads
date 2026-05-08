-- scs_arg_resolver.ads
with Command_Types;
with System;

package Scs_Arg_Resolver is
   type T is abstract tagged null record;
   type T_Access is access constant T'Class;

   function Resolve (
      Resolver : T;
      Data     : System.Address  -- caller passes Arg'Address
   ) return Command_Types.Command_Arg_Buffer_Type is abstract;
end Scs_Arg_Resolver;