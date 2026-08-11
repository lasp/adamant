--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Zero_Divider_Cpp_Reciprocal;
with Printable_History;
with Command_Response.Representation;
with Sys_Time.Representation;
with Event.Representation;
with Event;
with Packed_U32.Representation;
with Packed_I32.Representation;
with Packed_F32.Representation;
with Packed_Magic_Number.Representation;
with Invalid_Command_Info.Representation;
with Packed_Exception_Occurrence.Representation;

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
package Component.Zero_Divider_Cpp.Implementation.Tester is

   use Component.Zero_Divider_Cpp_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Raising_Exception_In_Cpp_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Raise_Exception_In_Cpp_No_Exception_History_Package is new Printable_History (Natural, Natural'Image);
   package Int_Dividing_By_Zero_In_Cpp_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Int_Divide_By_Zero_No_Exception_History_Package is new Printable_History (Packed_I32.T, Packed_I32.Representation.Image);
   package Fp_Dividing_By_Zero_In_Cpp_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Fp_Divide_By_Zero_No_Exception_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Invalid_Magic_Number_History_Package is new Printable_History (Packed_Magic_Number.T, Packed_Magic_Number.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Packet history packages:
   package Last_Chance_Handler_Packet_History_Package is new Printable_History (Packed_Exception_Occurrence.T, Packed_Exception_Occurrence.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Zero_Divider_Cpp_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Zero_Divider_Cpp.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Raising_Exception_In_Cpp_History : Raising_Exception_In_Cpp_History_Package.Instance;
      Raise_Exception_In_Cpp_No_Exception_History : Raise_Exception_In_Cpp_No_Exception_History_Package.Instance;
      Int_Dividing_By_Zero_In_Cpp_History : Int_Dividing_By_Zero_In_Cpp_History_Package.Instance;
      Int_Divide_By_Zero_No_Exception_History : Int_Divide_By_Zero_No_Exception_History_Package.Instance;
      Fp_Dividing_By_Zero_In_Cpp_History : Fp_Dividing_By_Zero_In_Cpp_History_Package.Instance;
      Fp_Divide_By_Zero_No_Exception_History : Fp_Divide_By_Zero_No_Exception_History_Package.Instance;
      Invalid_Magic_Number_History : Invalid_Magic_Number_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Packet histories:
      Last_Chance_Handler_Packet_History : Last_Chance_Handler_Packet_History_Package.Instance;
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
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A Raise_Exception_In_Cpp command was received and the magic number was correct.
   -- The exception will be raised in N milliseconds, where N is provided as the
   -- event parameter.
   overriding procedure Raising_Exception_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T);
   -- The C++ exception raise did not propagate as expected. This event should never
   -- fire under normal operation and indicates the target does not propagate C++
   -- exceptions to Ada as expected.
   overriding procedure Raise_Exception_In_Cpp_No_Exception (Self : in out Instance);
   -- An Int_Divide_By_Zero_In_Cpp command was received and the magic number was
   -- correct. The division will occur in N milliseconds, where N is provided as the
   -- event parameter.
   overriding procedure Int_Dividing_By_Zero_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T);
   -- The integer divide by zero in C++ returned to Ada without raising an exception.
   -- This is one of the outcomes the command exists to discover, not an anomaly. The
   -- parameter is the raw result returned by C++.
   overriding procedure Int_Divide_By_Zero_No_Exception (Self : in out Instance; Arg : in Packed_I32.T);
   -- An Fp_Divide_By_Zero_In_Cpp command was received and the magic number was
   -- correct. The floating point division will occur in N milliseconds, where N is
   -- provided as the event parameter.
   overriding procedure Fp_Dividing_By_Zero_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T);
   -- The floating point divide by zero in C++ returned to Ada without raising an
   -- exception. This is one of the outcomes the command exists to discover, not an
   -- anomaly. The parameter is the raw result returned by C++.
   overriding procedure Fp_Divide_By_Zero_No_Exception (Self : in out Instance; Arg : in Packed_F32.T);
   -- A command was received, but the magic number was incorrect. A magic number
   -- outside the range the type permits never reaches this event, it is rejected by
   -- command validation and reported as an invalid command instead.
   overriding procedure Invalid_Magic_Number (Self : in out Instance; Arg : in Packed_Magic_Number.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    The packet listed here is not actually produced by this component, but instead
   --    should be produced by the implementation of the Last_Chance_Handler. This
   --    packet definition exists to ensure that the packet gets reflected in the
   --    documentation and ground system definitions.
   -- This packet contains information regarding an exception occurrence that
   -- triggers the Last_Chance_Handler to get invoked. This packet is not produced
   -- directly by this component, and should be produced by the last chance handler
   -- implementation. This packet definition exists to ensure that the packet gets
   -- reflected in the documentation and ground system definitions. For the ground
   -- system to identify the packet, the APID assigned to this definition must be
   -- coordinated with the APID the last chance handler implementation emits.
   overriding procedure Last_Chance_Handler_Packet (Self : in out Instance; Arg : in Packed_Exception_Occurrence.T);

end Component.Zero_Divider_Cpp.Implementation.Tester;
