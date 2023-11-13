-- String representation package for Delta_Time
package Delta_Time.Pretty is

   -- Return pretty string representation of Delta_Time component
   function Image (Arg : in Delta_Time.T; Sec_Padding : in Integer := -1) return String;

end Delta_Time.Pretty;
