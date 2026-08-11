--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Zerodividercpp_C_H;
use Zerodividercpp_C_H;

-- This component provides commands that exercise C++ fault and arithmetic
-- behavior so that a target's response to them can be observed. The
-- `Raise_Exception_in_Cpp` command explicitly raises a C++ exception. When a
-- user-implemented C++ Termination Handler (TH) is configured to forward
-- termination events to the Ada Last Chance Handler (LCH), this command allows
-- verification that the exception propagation pathway is functioning correctly.
-- The `Int_Divide_By_Zero_In_Cpp` and `Fp_Divide_By_Zero_In_Cpp` commands are
-- informational rather than authoritative. Each performs a division by zero in
-- C++ and reports what the target does with the result. That outcome depends on
-- the target architecture, the compiler, and the compiler and runtime flags the
-- project is built with, so this component makes no claim about which outcome any
-- particular configuration produces. Integer division by zero is undefined
-- behavior in C++, so a target may trap or may return a value. Floating point
-- division by zero on a target that implements IEEE 754 typically yields a signed
-- infinity for a non-zero dividend and a NaN for a zero dividend, though whether
-- such a value reaches Ada or raises an exception on the way is itself
-- configuration dependent. Whenever a division returns to Ada without raising an
-- exception, the returned value is reported in an event. Run these commands on
-- the intended target to establish how that configuration behaves.
package Component.Zero_Divider_Cpp.Implementation is

   -- The component class instance record:
   type Instance is new Zero_Divider_Cpp.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The magic number and the time to sleep before each command executes are
   -- provided at instantiation.
   --
   -- Init Parameters:
   -- Magic_Number : Magic_Number_Type - As commands to this component crash the
   -- system, provide this number as the key value of the safety interlock mechanism
   -- that guards against unintentional execution of commands in this component.
   -- Sleep_Before_Execute_Ms : Natural - The number of milliseconds to sleep after
   -- receiving a command before executing its implementation. This allows time for
   -- any events to be written by the component, if desired.
   --
   overriding procedure Init (Self : in out Instance; Magic_Number : in Magic_Number_Type; Sleep_Before_Execute_Ms : in Natural := 1_000);

private

   -- The component class instance record:
   type Instance is new Zero_Divider_Cpp.Base_Instance with record
      -- This component is semi-stateless. Magic_Number is passed through and maintained
      -- in the zeroDividerCpp class in the C++ in the Init.
      Sleep_Before_Execute_Ms : Natural := 1_000;
      Zero_Divider_Cpp : access Zerodividercpp := null;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Zero Divider Cpp component.
   -- Performs an integer division by zero in C++ and reports the outcome. Integer
   -- division by zero is undefined behavior in C++, so what happens depends on the
   -- target and the flags the project is built with. A target may trap, or may
   -- return a value that is reported in an event. This command is informational, run
   -- it to establish what a given configuration does. You must provide the correct
   -- value for the magic number and an integer dividend for this command to execute.
   overriding function Int_Divide_By_Zero_In_Cpp (Self : in out Instance; Arg : in Int_Divide_By_Zero_In_Cpp_Arg.T) return Command_Execution_Status.E;
   -- Performs a floating point division by zero in C++ and reports the outcome. A
   -- target that implements IEEE 754 typically produces a signed infinity for a non-
   -- zero dividend and a NaN for a zero dividend, but whether such a value reaches
   -- Ada or raises an exception on the way depends on the target and the flags the
   -- project is built with. This command is informational, run it to establish what
   -- a given configuration does. You must provide the correct value for the magic
   -- number and a floating point dividend for this command to execute.
   overriding function Fp_Divide_By_Zero_In_Cpp (Self : in out Instance; Arg : in Fp_Divide_By_Zero_In_Cpp_Arg.T) return Command_Execution_Status.E;
   -- Raises a standard exception in C++. You must provide the correct value for the
   -- magic number argument of this command for it to be executed.
   overriding function Raise_Exception_In_Cpp (Self : in out Instance; Arg : in Packed_Magic_Number.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Zero_Divider_Cpp.Implementation;
