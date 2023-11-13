with Ada.Task_Identification;
with System;

-- This package contains data types related to Adamant tasks
package Task_Types is

   type Task_Number is new Natural range 0 .. 65_535;

   type Task_Info is record
      -- The Adamant task identifier
      Number : Task_Number := 0;
      -- The Ada runtime task identifier
      Id : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
      -- The Ada task priority
      Priority : System.Priority;
      -- The start address of the task stack:
      Stack_Address : System.Address := System.Null_Address;
      -- The stack size of the task:
      Stack_Size : Natural := 0;
      -- The start address of the task stack:
      Secondary_Stack_Address : System.Address := System.Null_Address;
      -- The secondary stack size of the task:
      Secondary_Stack_Size : Natural := 0;
      -- The maximum usage of the secondary stack in bytes:
      Secondary_Stack_Max_Usage : Natural := 0;
   end record;

   type Task_Info_Access is access all Task_Info;
   type Task_Info_List is array (Natural range <>) of not null Task_Info_Access;
   type Task_Info_List_Access is access all Task_Info_List;

end Task_Types;
