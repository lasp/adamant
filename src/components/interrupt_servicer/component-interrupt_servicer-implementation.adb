--------------------------------------------------------------------------------
-- Interrupt_Servicer Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Interrupt_Servicer.Implementation is

   ---------------------------------------
   -- Cycle function:
   ---------------------------------------
   overriding procedure Cycle (Self : in out Instance) is
      Interrupt_Data : Interrupt_Data_Type;
   begin
      -- Wait for the interrupt to release this task:
      Self.The_Signal.Wait (Interrupt_Data);

      -- Get the time of the interrupt and store it in the interrupt data:
      if Self.Is_Sys_Time_T_Get_Connected then
         Set_Interrupt_Data_Time (Interrupt_Data, Self.Sys_Time_T_Get);
      end if;

      -- Send the data out:
      Self.Interrupt_Data_Type_Send (Interrupt_Data);
   end Cycle;

end Component.Interrupt_Servicer.Implementation;
