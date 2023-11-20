package body Simple_Package is

   function Add_Two_Numbers (Left : in Integer; Right : in Integer) return Integer is
   begin
      return Left + Right;
   end Add_Two_Numbers;

end Simple_Package;
