--------------------------------------------------------------------------------
-- Tick_Divider Component Implementation Body
--------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package body Component.Tick_Divider.Implementation is

   ---------------------------------------
   -- Initialization procedure:
   ---------------------------------------
   -- This initialization function is used to set the divider values for the
   -- component. An entry of 0 disables that connector from ever being invoked.
   --
   -- Init Parameters:
   -- Dividers : Divider_Array_Type_Access - An access to an array of dividers used
   -- to determine the tick rate of each Tick_T_Send connector.
   -- Tick_Source : Tick_Source_Type - Configures whether to use internal counter or
   -- incoming tick's Count field for division.
   --
   overriding procedure Init (Self : in out Instance; Dividers : in not null Divider_Array_Type_Access; Tick_Source : in Tick_Source_Type := Internal) is
      Divider : Interfaces.Unsigned_32;
   begin
      pragma Assert (Dividers'First = Self.Connector_Tick_T_Send'First, "The length of the dividers array must match the length of the connector array!");
      pragma Assert (Dividers'Last = Self.Connector_Tick_T_Send'Last, "The length of the dividers array must match the length of the connector array!");
      pragma Assert (Dividers'Length = Self.Connector_Tick_T_Send'Length, "The length of the dividers array must match the length of the connector array!");

      -- Copy the dividers and tick source to the component:
      Self.Dividers := Dividers;
      Self.Tick_Source := Tick_Source;

      -- If the Tick_Source is Internal, then we need to calculate a good rollover value.
      case Self.Tick_Source is
         when Internal =>
            -- Calculate the Max_Count. This is the rollover value for count. To make sure
            -- rollover does not skip ticks for any components we need to select a Max_Count
            -- value that is divisible by all the divisors. The simplest way to do this is
            -- to simply multiply all the divisors together.
            Self.Max_Count := 1;
            for Index in Self.Dividers'Range loop
               -- Ignore zero entries, since this special value means that the connector index
               -- is disabled, thus it should not be included in the calculation.
               Divider := Self.Dividers (Index);
               if Divider > 0 then
                  Self.Max_Count := @ * Divider;
               end if;
            end loop;

            -- Make sure Max_Count doesn't overflow on increment (leave room for +1).
            pragma Assert (Self.Max_Count < Interfaces.Unsigned_32'Last);
         when Tick_Counter =>
            null; -- Max_Count will be unused in this case, so do nothing with it.
      end case;
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a periodic Tick from an external component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Divider : Interfaces.Unsigned_32;
      Current_Count : Interfaces.Unsigned_32;
   begin
      -- Determine which count to use based on Tick_Source configuration
      case Self.Tick_Source is
         when Internal =>
            Current_Count := Self.Count;

            -- Increment and roll over the internal count if necessary. The
            -- rollover value is the product of the divisors.
            Self.Count := (@ + 1) mod Self.Max_Count;
            -- ^ Note: this will fail with a divide by zero error if init is
            -- never called. This behavior is by design, to alert the developer.
         when Tick_Counter =>
            Current_Count := Arg.Count;

            -- In this case, we trust that the external Arg.Count will be
            -- incremented for the next call to this handler.
      end case;

      -- Call each connected send connector in turn, if that divider is ready:
      for Index in Self.Connector_Tick_T_Send'Range loop
         Divider := Self.Dividers (Index);
         -- If the divider is positive, the connector is connected, and
         -- count is divisible by the divider, then invoke the connector.
         if Divider > 0 and then Self.Connector_Tick_T_Send (Index).Is_Connected and then (Current_Count mod Divider) = 0 then
            -- Send the tick and check the result:
            Self.Tick_T_Send (Index, Arg);
         end if;
      end loop;
   end Tick_T_Recv_Sync;

   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Index : in Tick_T_Send_Index; Arg : in Tick.T) is
   begin
      if Self.Is_Event_T_Send_Connected then
         Self.Event_T_Send (Self.Events.Component_Has_Full_Queue (Self.Sys_Time_T_Get, (Dropped_Tick => Arg, Index => Index)));
      end if;
   end Tick_T_Send_Dropped;

end Component.Tick_Divider.Implementation;
