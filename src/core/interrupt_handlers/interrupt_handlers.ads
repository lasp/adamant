with Ada.Interrupts;
with System;

-- This package contains a collection of protected objects that attach to interrupts. Each protected interrupt
-- handler object has different properties which might make it better suited for a certain job. Each interrupt
-- handler object receives a custom interrupt handler procedure (written by the user) which is called whenever
-- an interrupt is received. This handler is provided an in out parameter of the generic type T. This entire
-- package is generic, such that the user can instantiate it with the desired data type to be passed between their
-- user written interrupt handler procedure and the protected object itself. In this way, this package provides
-- a thread-safe pattern for handling interrupts while offering enough type flexibility to be used for any
-- foreseeable interrupt handling pattern. If your pattern doesn't fit with one of the protected types below
-- consider adding a new type to the package to grow the collection to meet more use cases.
generic
   -- The generic type which the custom interrupt handler function will use/return. If you are
   -- unsure what to make this type, consider making it a counter, or a timestamp when the
   -- interrupt handler was triggered.
   type T is private;
package Interrupt_Handlers is

   -- Definitions for custom interrupt procedure type. This type of procedure can
   -- be passed by a user to the instantiation of a handler (defined below) and will
   -- be executed in the interrupt handler itself when an interrupt is received.
   -- Using this method, platform or hardware specific code that needs to be executed
   -- within interrupt handlers can be wrapped by an Interrupt_Procedure_Type and passed
   -- to a handler.
   --
   -- The interrupt procedure type contains an in out parameter of generic type T. This
   -- can be used to pass/return data to/from a custom interrupt procedure.
   type Interrupt_Procedure_Type is not null access procedure (Data : in out T);

   -- Definition for a null interrupt procedure, which does nothing. Use this when
   -- you don't need to execute any specific code within the interrupt handler.
   procedure Null_Interrupt_Procedure (Data : in out T) is null;
   Null_Interrupt_Procedure_Access : Interrupt_Procedure_Type := Null_Interrupt_Procedure'Access;

   -- This interrupt handler can be used to implement a signal/wait pattern for a
   -- task with an infinite loop.
   protected type Task_Signal (Pri : System.Interrupt_Priority; Int_Id : Ada.Interrupts.Interrupt_ID; Custom_Procedure : Interrupt_Procedure_Type) is
      -- A "no-op" procedure which does not return until an interrupt has been signaled.
      -- This can be used to release a task in an infinite loop.
      entry Wait (Data : out T);
      function Get_Data return T;
      procedure Set_Data (Data : in T);
      pragma Interrupt_Priority (Pri);
   private
      -- The actual interrupt handler procedure, which gets called when
      -- the interrupt is received.
      procedure Handler;
      pragma Attach_Handler (Handler, Int_Id);
      -- Internal boolean used to release the "Wait" procedure.
      Signaled : Boolean := False;
      -- Internal type for storing the generic data:
      Internal_Data : T;
   end Task_Signal;

   -- This interrupt handler simply updates an internal count that a task can query and
   -- reset. This is useful for letting a task know that an interrupt has occurred without
   -- blocking the task (as the above handler does).
   protected type Interrupt_Counter (Pri : System.Interrupt_Priority; Int_Id : Ada.Interrupts.Interrupt_ID; Custom_Procedure : Interrupt_Procedure_Type) is
      -- Function which returns the current count without modifying it:
      procedure Get_Count_And_Reset (The_Count : out Natural; Data : out T);
      procedure Reset_Count;
      function Get_Count return Natural;
      function Get_Data return T;
      procedure Set_Data (Data : in T);
      pragma Interrupt_Priority (Pri);
   private
      -- The actual interrupt handler procedure, which gets called when
      -- the interrupt is received.
      procedure Handler;
      pragma Attach_Handler (Handler, Int_Id);
      -- Internal count variable.
      Count : Natural := Natural'First; -- Variable will never get higher than Natural'Last per implementation
      -- Internal type for storing the generic data:
      Internal_Data : T;
   end Interrupt_Counter;

end Interrupt_Handlers;
