--------------------------------------------------------------------------------
-- Interrupt_Pender Component Tester Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Component.Interrupt_Pender_Reciprocal;
with History;
with Sys_Time;
with System;
with Ada.Interrupts.Names;

-- Invokee Connector Includes:

-- This is the Interrupt Pender component. It is attached to an interrupt and provides a connector which will block any component that invokes it until an interrupt is triggered. When an interrupt occurs the component will return a Tick to the waiting component. This component should be made passive in order to function properly.
generic
package Component.Interrupt_Pender.Implementation.Tester is

   Interrupt_Id : constant Ada.Interrupts.Interrupt_ID := Ada.Interrupts.Names.SIGUSR1;

   package Interrupt_Pender_Package is new Component.Interrupt_Pender_Reciprocal (Interrupt_Data_Type);
   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new History (Sys_Time.T);

   -- Component class instance:
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type) is new Interrupt_Pender_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Interrupt_Pender.Implementation.Instance (Custom_Interrupt_Procedure, Interrupt_Id, System.Interrupt_Priority'Last);
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

end Component.Interrupt_Pender.Implementation.Tester;
