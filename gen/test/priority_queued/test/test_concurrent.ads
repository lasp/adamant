with Component.Priority_Queued.Implementation.Tester;
with Ada.Synchronous_Task_Control;
with Task_Types;

package Test_Concurrent is
   Tester : Component.Priority_Queued.Implementation.Tester.Instance;

   -- Task creation:
   Component_Signal : aliased Ada.Synchronous_Task_Control.Suspension_Object;
   Task_Info : aliased Task_Types.Task_Info;
   My_Component_Task : Component.Active_Task (Task_Info'Access, Tester.Component_Instance'Unrestricted_Access, Component_Signal'Access, 4, 3_000_000, 3_000);

   procedure Test;
end Test_Concurrent;
