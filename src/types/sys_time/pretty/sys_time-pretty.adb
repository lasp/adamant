-- Standard includes:
with String_Util; use String_Util;
with Interfaces; use Interfaces;

package body Sys_Time.Pretty is

   -- Return pretty string representation of Sys_Time component
   function Image (Arg : in Sys_Time.T; Sec_Padding : in Integer := -1) return String is
      Nanoseconds : constant Unsigned_32 := Unsigned_32 ((Unsigned_64 (Arg.Subseconds) * 1_000_000_000) / (Unsigned_64 (Subseconds_Type'Last) + 1));
   begin
      if Sec_Padding < 0 then
         return Trim_Both (Arg.Seconds'Image) & "." & Pad_Zeros (Trim_Both (Nanoseconds'Image), 9);
      else
         return Pad_Zeros (Trim_Both (Arg.Seconds'Image), Sec_Padding) & "." & Pad_Zeros (Trim_Both (Nanoseconds'Image), 9);
      end if;
   end Image;

end Sys_Time.Pretty;
