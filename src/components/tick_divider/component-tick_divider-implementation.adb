--------------------------------------------------------------------------------
-- Tick_Divider Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Tick_Divider.Implementation is

   ---------------------------------------
   -- Initialization procedure:
   ---------------------------------------
   -- This initialization function is used to set the divider values for the component. An entry of 0 disables that connector from ever being invoked.
   --
   overriding procedure Init (Self : in out Instance; Dividers : in not null Divider_Array_Type_Access) is
      Divider : Natural;
   begin
      pragma Assert (Dividers'First = Self.Connector_Tick_T_Send'First, "The length of the dividers array must match the length of the connector array!");
      pragma Assert (Dividers'Last = Self.Connector_Tick_T_Send'Last, "The length of the dividers array must match the length of the connector array!");
      pragma Assert (Dividers'Length = Self.Connector_Tick_T_Send'Length, "The length of the dividers array must match the length of the connector array!");

      -- Copy the dividers to the component:
      Self.Dividers := Dividers;

      -- Calculate the max_Count. This is the rollover value for count. To make sure
      -- rollover does not skip ticks for any components we need to select a max_Count
      -- value that is divisible by all the divisors. The simplest way to do this is
      -- to simply multiply all the divisors together.
      Self.Max_Count := 1;
      for Index in Self.Dividers'Range loop
         -- Ignore zero entries, since this special value means that the connector index
         -- is disabled, thus it should not be included in the calculation.
         Divider := Self.Dividers (Index);
         if Divider > 0 then
            Self.Max_Count := Self.Max_Count * Divider;
         end if;
      end loop;

      -- Make sure Max_Count is less than Natural'Last otherwise Self.Count will throw
      -- a Constraint_Error on increment.
      pragma Assert (Self.Max_Count < Natural'Last);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a periodic Tick from an external component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Divider : Natural;
   begin
      -- Call each connected send connector in turn, if that divider is ready:
      for Index in Self.Connector_Tick_T_Send'Range loop
         Divider := Self.Dividers (Index);
         -- If the divider is positive, the connector is connected, and
         -- count is divisible by the divider, then invoke the connector.
         if Divider > 0 and then Self.Connector_Tick_T_Send (Index).Is_Connected and then (Self.Count mod Divider) = 0 then
            -- Send the tick and check the result:
            Self.Tick_T_Send (Index, Arg);
         end if;
      end loop;

      -- Roll over the count if necessary. The rollover value is the
      -- product of the divisors.
      -- Note: this will fail with a divide by zero error if init is
      -- never called. This behavior is by design.
      -- This increment will overflow because Self.Max_Count < Natural'Last
      -- according to check in the Init.
      Self.Count := (Self.Count + 1) mod Self.Max_Count;
   end Tick_T_Recv_Sync;

   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Index : in Tick_T_Send_Index; Arg : in Tick.T) is
   begin
      if Self.Is_Event_T_Send_Connected then
         Self.Event_T_Send (Self.Events.Component_Has_Full_Queue (Self.Sys_Time_T_Get, (Dropped_Tick => Arg, Index => Index)));
      end if;
   end Tick_T_Send_Dropped;

end Component.Tick_Divider.Implementation;
