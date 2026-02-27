procedure Test is
   --   Declare then export an Integer entity called Num_From_Ada using the
   -- "with" syntax.
   My_Num : Integer := 10
     with Export => True, Convention => C, External_Name => "num_from_Ada";

   --   Declare an Ada function spec for Get_Num, then use
   --   C function get_num for the implementation.
   function Get_Num return Integer;
   -- Use the pragma Import syntax.
   pragma Import (Convention => C, Entity => Get_Num, External_Name => "get_num");

   --   Declare an Ada procedure spec for Print_Num, then use
   --   C function print_num for the implementation.
   --   Use the "with" syntax.
   procedure Print_Num (Num : Integer)
      with Import => True, Convention => C, External_Name => "print_num";
begin
   Print_Num (Get_Num);
end Test;
