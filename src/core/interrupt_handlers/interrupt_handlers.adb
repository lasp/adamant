package body Interrupt_Handlers is

   protected body Task_Signal is
      -- A procedure which waits for an interrupt to be handled.
      entry Wait (Data : out T) when Signaled is
      begin
         -- Return data to the user:
         Data := Internal_Data;
         Signaled := False;
      end Wait;

      function Get_Data return T is
      begin
         return Internal_Data;
      end Get_Data;

      procedure Set_Data (Data : in T) is
      begin
         Internal_Data := Data;
      end Set_Data;

      -- The actual interrupt handler procedure, which gets called when
      -- the interrupt is received.
      procedure Handler is
      begin
         Custom_Procedure.all (Internal_Data);
         Signaled := True;
      end Handler;
   end Task_Signal;

   protected body Interrupt_Counter is

      procedure Get_Count_And_Reset (The_Count : out Natural; Data : out T) is
      begin
         -- Return the count to the user and reset it to 0:
         The_Count := Count;
         Count := Natural'First;
         Data := Internal_Data;
      end Get_Count_And_Reset;

      procedure Reset_Count is
      begin
         -- Reset the count to 0:
         Count := Natural'First;
      end Reset_Count;

      function Get_Count return Natural is
      begin
         -- Return the count without modifying its value:
         return Count;
      end Get_Count;

      function Get_Data return T is
      begin
         return Internal_Data;
      end Get_Data;

      procedure Set_Data (Data : in T) is
      begin
         Internal_Data := Data;
      end Set_Data;

      procedure Handler is
      begin
         Custom_Procedure.all (Internal_Data);
         -- Increment the count:
         if Count /= Natural'Last then
            Count := @ + 1;
         end if;
      end Handler;

   end Interrupt_Counter;

end Interrupt_Handlers;
