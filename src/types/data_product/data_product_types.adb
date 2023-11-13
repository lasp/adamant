with Sys_Time.Arithmetic;

package body Data_Product_Types is

   function Check_Data_Product_Stale (Timestamp : in Sys_Time.T; Stale_Reference : in Sys_Time.T; Stale_Limit : in Ada.Real_Time.Time_Span) return Stale_Status is
      use Sys_Time.Arithmetic;
      Too_Old_Time : Sys_Time.T;
      Time_Stat : constant Sys_Time_Status := Sys_Time.Arithmetic.Subtract (Left => Stale_Reference, Right => Stale_Limit, Result => Too_Old_Time);
   begin
      -- Check the time calculation status. If something went wrong with the calculation, likely
      -- underflow, then we need to assume that the data dependency is stale.
      case Time_Stat is
         when Success =>
            null; -- Continue on.
         when Underflow =>
            return Stale;
         when Overflow =>
            return Stale;
      end case;

      -- OK the time calculation is valid. Check to see if the data product
      -- is too old.
      if Timestamp < Too_Old_Time then
         return Stale;
      else
         return Success;
      end if;
   end Check_Data_Product_Stale;

end Data_Product_Types;
