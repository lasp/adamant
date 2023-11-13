-- Standard includes:
with Sys_Time.Pretty;
with Ada.Unchecked_Conversion;
package body Delta_Time.Pretty is

   -- Conversion functions between time delta and sys_time. They have the same representation
   -- so this conversion should be fine. The two have different semantics, but using this conversion
   -- allows us to reuse code from the more complicated, but very well tested, Sys_Time.Arithmetic package.
   function Delta_Time_To_Sys_Time is new Ada.Unchecked_Conversion (Source => Delta_Time.T, Target => Sys_Time.T);

   -- Return pretty string representation of Delta_Time component
   function Image (Arg : in Delta_Time.T; Sec_Padding : in Integer := -1) return String is
   begin
      return Sys_Time.Pretty.Image (Delta_Time_To_Sys_Time (Arg), Sec_Padding);
   end Image;

end Delta_Time.Pretty;
