with Ada.Task_Termination;
with Ada.Task_Identification;
with Ada.Exceptions;

-- By default if an Ada task terminates, it does so silently. This package
-- provides a task termination handler to ensure that tasks print a visible
-- error message when terminating.
package Unit_Test_Termination_Handler is
   -- Elaborate the body which will set up the task termination handlers
   -- for all library level tasks. This is done to ensure that the termination
   -- handler is set up before the tasks are started, thus eliminating a race
   -- condition;
   pragma Elaborate_Body;

   -- Note: This task termination prototype will only work on a native
   -- platform. Ada.Task_Termination has a different package spec for
   -- embedded platforms that use Ravenscar.
   protected Task_Termination is
      procedure Handler (Cause : Ada.Task_Termination.Cause_Of_Termination; T : Ada.Task_Identification.Task_Id; X : Ada.Exceptions.Exception_Occurrence);
   end Task_Termination;
end Unit_Test_Termination_Handler;
