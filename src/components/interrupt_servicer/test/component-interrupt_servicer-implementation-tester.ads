--------------------------------------------------------------------------------
-- Interrupt_Servicer Component Tester Spec
--------------------------------------------------------------------------------

with Component.Interrupt_Servicer_Reciprocal;
with History;
with Sys_Time;
with System;
with Ada.Interrupts.Names;

-- This is the Interrupt Servicer component. It is attached to an interrupt and sends out a Tick.T everytime the interrupt is triggered. This component MUST be made active in order to function properly.
generic
package Component.Interrupt_Servicer.Implementation.Tester is

   Interrupt_Id : constant Ada.Interrupts.Interrupt_ID := Ada.Interrupts.Names.SIGUSR1;

   package Interrupt_Servicer_Package is new Component.Interrupt_Servicer_Reciprocal (Interrupt_Data_Type);
   -- Invoker connector history packages:
   package Interrupt_Data_Type_Recv_Sync_History_Package is new History (Interrupt_Data_Type);
   package Sys_Time_T_Return_History_Package is new History (Sys_Time.T);

   -- Component class instance:
   type Instance (Custom_Interrupt_Procedure : Custom_Interrupt_Handler_Package.Interrupt_Procedure_Type) is new Interrupt_Servicer_Package.Base_Instance with record
      Component_Instance : aliased Component.Interrupt_Servicer.Implementation.Instance (Custom_Interrupt_Procedure, Interrupt_Id, System.Interrupt_Priority'Last);
      -- Connector histories:
      Interrupt_Data_Type_Recv_Sync_History : Interrupt_Data_Type_Recv_Sync_History_Package.Instance;
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
   -- The tick send connection.
   overriding procedure Interrupt_Data_Type_Recv_Sync (Self : in out Instance; Arg : in Interrupt_Data_Type);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

end Component.Interrupt_Servicer.Implementation.Tester;
