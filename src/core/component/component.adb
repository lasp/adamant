with Task_Util;

package body Component is

   task body Active_Task is
   begin
      -- Initialize the stack's task_data and stack.
      Task_Util.Initialize_Task (Task_Data, Pri, Stack_Size, Secondary_Stack_Size);

      -- Wait to start until signaled:
      Ada.Synchronous_Task_Control.Suspend_Until_True (Signal.all);
      while not Ada.Synchronous_Task_Control.Current_State (Signal.all) loop
         -- Call the task procedure:
         Class_Self.all.Cycle;
         -- Update secondary stack usage:
         Task_Util.Update_Secondary_Stack_Usage (Task_Data);
      end loop;
   end Active_Task;

   function Get_Queue_Current_Percent_Used (Self : in out Core_Instance) return Basic_Types.Byte is
      pragma Annotate (GNATSAS, Intentional, "subp always fails",
         "Intentional - this subp should never be called on a component without a queue.");
      Ignore : Core_Instance renames Self;
   begin
      pragma Assert (False, "This component does not contain a queue because this subprogram was not overridden.");
      return Basic_Types.Byte'Last;
   end Get_Queue_Current_Percent_Used;

   function Get_Queue_Maximum_Percent_Used (Self : in out Core_Instance) return Basic_Types.Byte is
      pragma Annotate (GNATSAS, Intentional, "subp always fails",
         "Intentional - this subp should never be called on a component without a queue.");
      Ignore : Core_Instance renames Self;
   begin
      pragma Assert (False, "This component does not contain a queue because this subprogram was not overridden.");
      return Basic_Types.Byte'Last;
   end Get_Queue_Maximum_Percent_Used;

end Component;
