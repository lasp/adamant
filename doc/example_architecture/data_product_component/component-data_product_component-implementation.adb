--------------------------------------------------------------------------------
-- Data_Product_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Data_Product_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Get the timestamp:
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Send the counter data product:
      Self.Data_Product_T_Send (Self.Data_Products.Counter (Timestamp, (Value => Self.Count)));

      -- Send the last tick data product:
      Self.Data_Product_T_Send (Self.Data_Products.Last_Tick_Received (Timestamp, Arg));

      -- Increment the count:
      Self.Count := @ + 1;
   end Tick_T_Recv_Sync;

end Component.Data_Product_Component.Implementation;
