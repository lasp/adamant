with Task_Types;
with Ada.Synchronous_Task_Control;
with System;
with Basic_Types;

-- This is a base component. There is not much here, but
-- this provides a base type for which other types can
-- use to operate on more specific components.
package Component is

   -- Component Type:
   type Core_Instance is abstract tagged limited null record;
   type Class_Access is access all Core_Instance'Class;

   -- List type:
   type Component_List is array (Natural range <>) of Class_Access;
   type Component_List_Access is access all Component_List;

   -- Component abstract method, executed cyclically in task:
   procedure Cycle (Ignore : in out Core_Instance) is abstract;

   -- Task type for active components:
   task type Active_Task (
      Task_Data : Task_Types.Task_Info_Access;
      Class_Self : not null Class_Access;
      Signal : not null access Ada.Synchronous_Task_Control.Suspension_Object;
      Pri : System.Priority;
      Stack_Size : Natural;
      Secondary_Stack_Size : Natural
   ) -- Set the priority and stack size for the task:
      with Priority => Pri,
             Storage_Size => Stack_Size,
             Secondary_Stack_Size => Secondary_Stack_Size;

   -- Null method which can be overridden to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be overridden to do these things
   -- if necessary.
   procedure Set_Up (Self : in out Core_Instance) is null;

   --
   -- Some public subprograms implemented by all components:
   --

   -- Get the current percent usage of the component's internal queue, if there
   -- is one, else assert. This function is used by the queue monitor component
   -- as a backdoor convenience to bypass the connector system, reducing complexity
   -- of reporting queue usage for every component.
   function Get_Queue_Current_Percent_Used (Self : in out Core_Instance) return Basic_Types.Byte;

   -- Get the maximum percent usage of the component's internal queue, if there
   -- is one, else assert. This function is used by the queue monitor component
   -- as a backdoor convenience to bypass the connector system, reducing complexity
   -- of reporting queue usage for every component. i.e. "high water mark"
   function Get_Queue_Maximum_Percent_Used (Self : in out Core_Instance) return Basic_Types.Byte;

end Component;
