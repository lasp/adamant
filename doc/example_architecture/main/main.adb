with Ada.Real_Time; use Ada.Real_Time;
with Science_Assembly;
with Last_Chance_Handler;
with System;
pragma Unreferenced (Last_Chance_Handler);

procedure Main is
   -- Set the priority of this main procedure to the highest so that it
   -- takes precedence over any task during initialization.
   pragma Priority (System.Priority'Last);
   -- Define some variables helpful for sleeping.
   Wait_Time : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (1_000_000);
   Next_Time : Ada.Real_Time.Time;
begin
   -- Set up the assembly by calling the initialization
   -- phases in the correct order:
   Science_Assembly.Init_Base;
   Science_Assembly.Set_Id_Bases;
   Science_Assembly.Connect_Components;
   Science_Assembly.Init_Components;

   -- Wait for all the tasks to have reached their "wait" on the
   -- suspension object.
   declare
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Wait_Time;
   begin
      delay until Start_Time;
   end;

   -- All tasks are waiting, now start them all simultaneously.
   Science_Assembly.Start_Components;

   -- Give time for all the tasks to be up and running.
   declare
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Wait_Time;
   begin
      delay until Start_Time;
   end;

   -- Now run the set up procedure.
   Science_Assembly.Set_Up_Components;

   -- Loop indefinitely, doing nothing important...
   -- All the real work is done by the component tasks.
   Next_Time := Ada.Real_Time.Clock + Wait_Time;
   loop
      -- Wait for time:
      delay until Next_Time;
      Next_Time := @ + Wait_Time;
   end loop;
end Main;
