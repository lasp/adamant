-- String representation package for Sys_Time
package Sys_Time.Pretty is
   -- Return pretty string representation of Sys_Time component
   function Image (Arg : in Sys_Time.T; Sec_Padding : in Integer := -1) return String;

end Sys_Time.Pretty;
