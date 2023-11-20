with GNAT.Secondary_Stack_Info;

package body Secondary_Stack_Util is

   function Get_Secondary_Stack_Max_Usage return Natural is
      use GNAT.Secondary_Stack_Info;
   begin
      return Natural (SS_Get_Max);
   end Get_Secondary_Stack_Max_Usage;

end Secondary_Stack_Util;
