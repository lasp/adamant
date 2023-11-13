with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Text_IO; use Ada.Text_IO;

package body Unit_Test_Termination_Handler is

   -- Print out task termination information to the user:
   protected body Task_Termination is
      procedure Handler (Cause : Ada.Task_Termination.Cause_Of_Termination; T : Ada.Task_Identification.Task_Id; X : Ada.Exceptions.Exception_Occurrence) is
         use Ada.Task_Identification;
         use Ada.Exceptions;
      begin
         case Cause is
            when Normal =>
               null;
               --Put(Standard_Error, "Task: ");
               --Put(Standard_Error, Image(T));
               --Put_Line(Standard_Error, " exited.");
            when Abnormal =>
               Put (Standard_Error, "Task: ");
               Put (Standard_Error, Image (T));
               Put_Line (Standard_Error, " exited abnormally.");
            when Unhandled_Exception =>
               Put (Standard_Error, "Task: ");
               Put (Standard_Error, Image (T));
               Put_Line (Standard_Error, " exited due to an unhandled exception:");
               Put_Line (Exception_Information (X));
         end case;
      end Handler;
   end Task_Termination;

begin
   -- Set up the fallback handler during elaboration to make sure that
   -- it gets set up before any tasks get started.
   Set_Dependents_Fallback_Handler (Task_Termination.Handler'Access);
end Unit_Test_Termination_Handler;
