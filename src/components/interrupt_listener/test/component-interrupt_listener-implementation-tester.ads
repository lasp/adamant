--------------------------------------------------------------------------------
-- Interrupt_Listener Component Tester Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Component.Interrupt_Listener_Reciprocal;
with System;
with Ada.Interrupts.Names;

-- This is the Interrupt Listener component. This component contains an internal piece of data (of generic type) which should be altered by a custom interrupt procedure passed in at instantiation. External components can request the latest version of this data at any time. A common use for this component might be to manage a counter, where the custom procedure increments the count with each interrupt, and the requester of the count uses the count to determine if an interrupt has been received.
generic
package Component.Interrupt_Listener.Implementation.Tester is

   Interrupt_Id : constant Ada.Interrupts.Interrupt_ID := Ada.Interrupts.Names.SIGUSR1;

   package Interrupt_Listener_Package is new Component.Interrupt_Listener_Reciprocal (Interrupt_Data_Type);
   -- Component class instance:
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type) is new Interrupt_Listener_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Interrupt_Listener.Implementation.Instance (Custom_Interrupt_Procedure, Interrupt_Id, System.Interrupt_Priority'Last);
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

end Component.Interrupt_Listener.Implementation.Tester;
