with Basic_Types;
with Memory_Enums;

-- Simulates the downstream memory components from a library-level
-- task. The Jorvik profile forbids task objects local to a test
-- procedure, so one task serves the whole suite: a scenario arms it
-- with the test class, disarms it when done, and the final scenario
-- asks it to quit so the host binary can exit.
package Memory_Copier_Tests.Implementation.Simulator is

   -- Knobs the scenarios set to control what the task sends. There is
   -- no thread safety here... but this is testing code.
   Task_Send_Response : Boolean := False;
   Task_Send_Timeout : Boolean := False;
   Task_Response : Memory_Enums.Memory_Copy_Status.E := Memory_Enums.Memory_Copy_Status.Success;

   -- Region the task hands back in its responses:
   Sim_Bytes : aliased Basic_Types.Byte_Array := [0 .. 99 => 12];

   -- Sleep for a number of milliseconds. Used by the task and by
   -- scenarios that pace it:
   procedure Sleep (Ms : in Natural := 5);

   -- Arm the task with the running test's class so it may call the Tester:
   procedure Arm (Ptr : in Class_Access);
   -- Detach the task from the running test:
   procedure Disarm;
   -- Ask the task to exit so the host binary can terminate:
   procedure Request_Quit;
   -- Block until the task has seen a Disarm and gone idle:
   procedure Wait_Idle;

end Memory_Copier_Tests.Implementation.Simulator;
