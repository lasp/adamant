--------------------------------------------------------------------------------
-- Interrupt_Servicer Component Implementation Spec
--------------------------------------------------------------------------------

-- Discriminant Type Includes:
with Ada.Interrupts;
with System;

-- This is the Interrupt Servicer component. It is attached to an interrupt and sends out a Tick.T every time the interrupt is triggered. This component MUST be made active in order to function properly.
generic
package Component.Interrupt_Servicer.Implementation is

   -- The component class instance record:
   --
   -- Discriminant Parameters:
   -- Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type - A custom procedure to be called within the interrupt handler. The null procedure can be used here if no specific behavior is desired.
   -- Interrupt_Id : Ada.Interrupts.Interrupt_ID - Interrupt identifier number for interrupt
   -- Interrupt_Priority : System.Interrupt_Priority - Interrupt priority for interrupt
   --
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type; Interrupt_Id : Ada.Interrupts.Interrupt_ID; Interrupt_Priority : System.Interrupt_Priority) is new Interrupt_Servicer.Base_Instance with private;

private

   -- The component class instance record:
   --
   -- Discriminant Parameters:
   -- Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type - A custom procedure to be called within the interrupt handler. The null procedure can be used here if no specific behavior is desired.
   -- Interrupt_Id : Ada.Interrupts.Interrupt_ID - Interrupt identifier number for interrupt
   -- Interrupt_Priority : System.Interrupt_Priority - Interrupt priority for interrupt
   --
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type; Interrupt_Id : Ada.Interrupts.Interrupt_ID; Interrupt_Priority : System.Interrupt_Priority) is new Interrupt_Servicer.Base_Instance with record
      -- The internal task signal protected object.
      The_Signal : Custom_Interrupt_Handler_Package.Task_Signal (Interrupt_Priority, Interrupt_Id, Custom_Interrupt_Procedure);
   end record;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Interrupt_Data_Type_Send message is dropped due to a full queue.
   overriding procedure Interrupt_Data_Type_Send_Dropped (Self : in out Instance; Arg : in Interrupt_Data_Type) is null;

   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- This is an active component with no queue, so the
   -- cycle function for the component's task must be
   -- implemented here in the implementation class as
   -- a user defined custom function.
   overriding procedure Cycle (Self : in out Instance);

end Component.Interrupt_Servicer.Implementation;
