with Ada.Task_Identification;
with System.Storage_Elements;
with Secondary_Stack_Util;
with Interfaces;
with Stack_Margin;

package body Task_Util is

   -- Write a byte pattern to the stack.
   function Set_Stack_Pattern (Value : in Interfaces.Unsigned_8; Size : in Natural) return System.Address is
      use Interfaces;
      type Stack_Array is array (0 .. Size - 1) of Unsigned_8 with
         Volatile => True;
      The_Stack : Stack_Array with
         Volatile => True;
   begin
      for Byte of The_Stack loop
         -- Set the byte to the pattern.
         Byte := Value;
      end loop;
      -- Return the lowest address on the stack:
      return The_Stack'Address;
      pragma Annotate (GNATSAS, Intentional, "stack variable address escape", "Intentionally returning the address of a stack variable for info purposes.");
   end Set_Stack_Pattern;

   procedure Initialize_Task (Task_Data : in Task_Types.Task_Info_Access; Pri : in System.Priority; Stack_Size : in Natural; Secondary_Stack_Size : in Natural) is
      use System.Storage_Elements;
      -- Assume that the stack starts at the address of this variable.
      Stack_Start : constant Interfaces.Unsigned_32 := 16#DDDD_DDDD#;
   begin
      -- Make sure the stack size is greater than the minimum supported. The rationale here is to support
      -- a portable stack monitoring system we need some amount of margin. 2kb should always work for this.
      pragma Assert (Stack_Size >= 2_000, "Adamant supports a minimum stack size of 2kb at this time.");

      -- Make sure that the stack size is larger than the stack margin:
      pragma Assert (Stack_Size > Stack_Margin.Margin, "The stack size (" & Natural'Image (Stack_Size) & " bytes) is not larger than the stack margin (" & Natural'Image (Stack_Margin.Margin) & " bytes). You need to increase the stack size for this task.");

      declare
         -- Set the "usable" stack size. This variable holds a conservative underestimate of the actual allocated stack size. It is
         -- the amount of stack size that will be monitored by the stack_monitor component. We don't expect the task to use more
         -- than this number. If it does, then the stack size should be increased.
         -- Note: The variable below may trigger a constraint error if the math goes negative. This is by design. The current assumption
         -- is that stack sizes for Adamant need to be ~2kb at a minimum to function correctly.
         Usable_Stack_Size : constant Natural := Stack_Size - Stack_Margin.Margin;
         -- Add a known pattern to the stack. This "painting" of the stack makes it easy to determine
         -- how much the stack has grown.
         Stack_End_Address : constant System.Address := Set_Stack_Pattern (Value => 16#CC#, Size => Usable_Stack_Size);
      begin
         -- Set items in the task data:
         Task_Data.all.Id := Ada.Task_Identification.Current_Task;
         Task_Data.all.Priority := Pri;
         -- Set the start of the stack to the address of the first variable of this task.
         Task_Data.all.Stack_Address := Stack_Start'Address;
         -- Assume stack grows down in memory:
         pragma Assert (Integer (Stack_Start'Address - Stack_End_Address) >= 0);
         -- The usable stack size is the start of the stack minus the end of the stack (which will be lower in memory).
         Task_Data.all.Stack_Size := Natural (Stack_Start'Address - Stack_End_Address);
         -- Make sure all our assumptions about size are true above.
         pragma Assert (Task_Data.all.Stack_Size <= Stack_Size);
         -- Set secondary stack info.
         Task_Data.all.Secondary_Stack_Address := System.Null_Address; -- Currently unused in Adamant.
         Task_Data.all.Secondary_Stack_Size := Secondary_Stack_Size;
         Update_Secondary_Stack_Usage (Task_Data);
      end;
   end Initialize_Task;

   procedure Update_Secondary_Stack_Usage (Task_Data : in Task_Types.Task_Info_Access) is
   begin
      Task_Data.all.Secondary_Stack_Max_Usage := Secondary_Stack_Util.Get_Secondary_Stack_Max_Usage;
   end Update_Secondary_Stack_Usage;

end Task_Util;
