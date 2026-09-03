with Basic_Types;
with Parameter_Enums;

-- Simulates the downstream parameter table components from a
-- library-level task. The Jorvik profile forbids task objects local
-- to a test procedure, so one task serves the whole suite: a scenario
-- arms it with the test class, disarms it when done, and the final
-- scenario asks it to quit so the host binary can exit.
package Ccsds_Parameter_Table_Router_Tests.Implementation.Simulator is

   -- Knobs the scenarios set to control what the task sends. There is
   -- no thread safety here... but this is testing code.
   Task_Send_Response : Boolean := False;
   Task_Response_Status : Parameter_Enums.Parameter_Table_Update_Status.E := Parameter_Enums.Parameter_Table_Update_Status.Success;
   Task_Send_Timeout : Boolean := False;
   Task_Responses_To_Send : Natural := 0;

   -- Parameter table byte array the task hands back in its responses:
   Sim_Bytes : aliased Basic_Types.Byte_Array := [0 .. 1023 => 16#AB#];

   -- Optional response schedule - If populated, the simulator reads successive
   -- statuses from this list and overrides Task_Response_Status for each response
   -- sent.
   Max_Schedule_Length : constant := 20;
   Response_Schedule : array (0 .. Max_Schedule_Length - 1) of Parameter_Enums.Parameter_Table_Update_Status.E :=
      [others => Parameter_Enums.Parameter_Table_Update_Status.Success];
   Schedule_Length : Natural := 0;
   Schedule_Index : Natural := 0;

   -- Arm the task with the running test's class so it may call the Tester:
   procedure Arm (Ptr : in Class_Access);
   -- Detach the task from the running test:
   procedure Disarm;
   -- Ask the task to exit so the host binary can terminate:
   procedure Request_Quit;
   -- Block until the task has seen a Disarm and gone idle:
   procedure Wait_Idle;

end Ccsds_Parameter_Table_Router_Tests.Implementation.Simulator;
