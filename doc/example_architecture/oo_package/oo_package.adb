package body Oo_Package is

   procedure Init (Self : in out Instance; N : in Integer) is
   begin
      -- Set our internal state variable to the input:
      Self.N := N;
   end Init;

   function Add_N (Self : in Instance; Left : in Integer) return Integer is
   begin
      return Left + Self.N;
   end Add_N;

end Oo_Package;
