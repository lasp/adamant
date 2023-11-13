-- We know that this is version dependant. We will need to update this if the
-- runtime implementation changes.
pragma Warnings (Off, """System.Secondary_Stack"" is an internal GNAT unit");
pragma Warnings (Off, "use of this unit is non-portable and version-dependent");
with System.Secondary_Stack;
pragma Warnings (On, """System.Secondary_Stack"" is an internal GNAT unit");
pragma Warnings (On, "use of this unit is non-portable and version-dependent");

package body Secondary_Stack_Util is

   function Get_Secondary_Stack_Max_Usage return Natural is
      use System.Secondary_Stack;
   begin
      return Natural (SS_Get_Max);
   end Get_Secondary_Stack_Max_Usage;

end Secondary_Stack_Util;
