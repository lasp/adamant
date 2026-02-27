--------------------------------------------------------------------------------
-- Interrupt_Pender Component Implementation Spec
--------------------------------------------------------------------------------

-- Discriminant Type Includes:
with Ada.Interrupts;
with System;

-- This is the Interrupt Pender component. It is attached to an interrupt and provides a connector which will block any component that invokes it until an interrupt is triggered. When an interrupt occurs the component will return a Tick to the waiting component. This component should be made passive in order to function properly.
generic
package Component.Interrupt_Pender.Implementation is

   -- The component class instance record:
   --
   -- Discriminant Parameters:
   -- Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type - A custom procedure to be called within the interrupt handler. The null procedure can be used here if no specific behavior is desired.
   -- Interrupt_Id : Ada.Interrupts.Interrupt_ID - Interrupt identifier number for interrupt
   -- Interrupt_Priority : System.Interrupt_Priority - Interrupt priority for interrupt
   --
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type; Interrupt_Id : Ada.Interrupts.Interrupt_ID; Interrupt_Priority : System.Interrupt_Priority) is new Interrupt_Pender.Base_Instance with private;

private

   -- The component class instance record:
   --
   -- Discriminant Parameters:
   -- Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type - A custom procedure to be called within the interrupt handler. The null procedure can be used here if no specific behavior is desired.
   -- Interrupt_Id : Ada.Interrupts.Interrupt_Id - Interrupt identifier number for interrupt
   -- Interrupt_Priority : System.Interrupt_Priority - Interrupt priority for interrupt
   --
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type; Interrupt_Id : Ada.Interrupts.Interrupt_ID; Interrupt_Priority : System.Interrupt_Priority) is new Interrupt_Pender.Base_Instance with record
      -- The internal task signal protected object.
      The_Signal : Custom_Interrupt_Handler_Package.Task_Signal (Interrupt_Priority, Interrupt_Id, Custom_Interrupt_Procedure);
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The tick wait connection.
   overriding function Wait_On_Interrupt_Data_Type_Return (Self : in out Instance) return Interrupt_Data_Type;

end Component.Interrupt_Pender.Implementation;
