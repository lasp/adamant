with Task_Types;
with System;

package Task_Util is

   -- Initialize the task_data structure and the task's stack.
   procedure Initialize_Task (Task_Data : in Task_Types.Task_Info_Access; Pri : in System.Priority; Stack_Size : in Natural; Secondary_Stack_Size : in Natural);

   -- Set the secondary stack high water mark in the task data:
   procedure Update_Secondary_Stack_Usage (Task_Data : in Task_Types.Task_Info_Access);

end Task_Util;
