package body Task_Synchronization is

   protected body Wait_Release_Object is
      entry Wait when Do_Release is
      begin
         Do_Release := False;
      end Wait;

      procedure Release is
      begin
         Do_Release := True;
      end Release;

      procedure Reset is
      begin
         Do_Release := False;
      end Reset;
   end Wait_Release_Object;

   protected body Wait_Release_Timeout_Object is
      entry Wait (Timed_Out : out Boolean) when Do_Release is
      begin
         Timed_Out := Do_Timeout;
         Do_Release := False;
      end Wait;

      procedure Release is
      begin
         Do_Timeout := False;
         Do_Release := True;
      end Release;

      procedure Timeout is
      begin
         -- If a task is blocked at the entry then activate
         -- the timeout, otherwise, don't do anything. This
         -- allows a task to poll this procedure, regardless
         -- of whether a task is waiting or not.
         if Wait'Count > 0 then
            Do_Timeout := True;
            Do_Release := True;
         end if;
      end Timeout;

      function Is_Waiting return Boolean is
      begin
         -- A task is blocked at the entry if this value
         -- is positive.
         return Wait'Count > 0;
      end Is_Waiting;

      procedure Reset is
      begin
         Do_Timeout := False;
         Do_Release := False;
      end Reset;

   end Wait_Release_Timeout_Object;

   protected body Wait_Release_Timeout_Counter_Object is

      procedure Set_Timeout_Limit (New_Timeout_Limit : in Natural) is
      begin
         Timeout_Limit := New_Timeout_Limit;
         Timeout_Count := 0;
      end Set_Timeout_Limit;

      entry Wait (Timed_Out : out Boolean) when Do_Release is
      begin
         Timed_Out := Do_Timeout;
         Do_Release := False;
      end Wait;

      procedure Release is
      begin
         Do_Timeout := False;
         Do_Release := True;
         Timeout_Count := 0;
      end Release;

      procedure Increment_Timeout_If_Waiting is
      begin
         -- If a task is blocked at the entry then activate
         -- the timeout, otherwise, don't do anything. This
         -- allows a task to poll this procedure, regardless
         -- of whether a task is waiting or not.
         if Wait'Count > 0 then
            -- If we are at or over the timeout limit then
            -- trigger a timeout condition.
            if Timeout_Count >= Timeout_Limit then
               Do_Timeout := True;
               Do_Release := True;
               -- Reset the timeout counter.
               Timeout_Count := 0;
            else
               -- Increment the timeout counter.
               Timeout_Count := @ + 1;
            end if;
         else
            -- If no task is waiting, then reset the
            -- timeout count.
            Timeout_Count := 0;
         end if;
      end Increment_Timeout_If_Waiting;

      procedure Timeout is
      begin
         -- If a task is blocked at the entry then activate
         -- the timeout, otherwise, don't do anything. This
         -- allows a task to poll this procedure, regardless
         -- of whether a task is waiting or not.
         if Wait'Count > 0 then
            Do_Timeout := True;
            Do_Release := True;
         end if;
         Timeout_Count := 0;
      end Timeout;

      function Is_Waiting return Boolean is
      begin
         -- A task is blocked at the entry if this value
         -- is positive.
         return Wait'Count > 0;
      end Is_Waiting;

      procedure Reset is
      begin
         Do_Timeout := False;
         Do_Release := False;
         Timeout_Count := 0;
      end Reset;

   end Wait_Release_Timeout_Counter_Object;

end Task_Synchronization;
