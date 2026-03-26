--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Zero_Divider_Cpp_Reciprocal;
with Printable_History;
with Command_Response.Representation;
with Sys_Time.Representation;
with Event.Representation;
with Packed_Exception_Occurrence.Representation;
with Event;
with Packed_U32.Representation;
with Packed_I32.Representation;
with Packed_F32.Representation;
with Invalid_Command_Info.Representation;

-- This component provides commands intended for testing propagation of C++ faults
-- through a user-implemented C++ Termination Handler (TH) to the Ada Last Chance
-- Handler (LCH). The `Raise_Exception_in_Cpp` command explicitly raises a C++
-- exception. When a TH is configured to forward termination events to the Ada
-- LCH, this command allows verification that the exception propagation pathway is
-- functioning correctly. The `Int_Divide_By_Zero_In_Cpp` command demos a
-- potential approach to the special case of integer division-by-zero handling. In
-- C++, division by zero is undefined behavior, and the resulting handling is
-- typically determined either by a customized C++ runtime or by target-specific
-- behavior. Many systems return a defined value (often NaN) rather than raising
-- an exception. To ensure such conditions propagate to the Ada LCH, the result of
-- the division can be returned to Ada and assigned to a constrained type whose
-- range excludes the expected return value. This constraint violation raises an
-- Ada exception which, if left unhandled, triggers the Last Chance Handler. The
-- `FP_Divide_By_Zero_In_Cpp` command performs a floating-point division by zero
-- in C++, which per IEEE 754 produces +infinity or -infinity. The result is
-- assigned to an Ada constrained float subtype that excludes infinities,
-- triggering a Constraint_Error that propagates to the Last Chance Handler.
package Component.Zero_Divider_Cpp.Implementation.Tester is

   use Component.Zero_Divider_Cpp_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Raising_Exception_In_Cpp_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Unexpected_Raising_Exception_In_Cpp_Result_History_Package is new Printable_History (Natural, Natural'Image);
   package Int_Dividing_By_Zero_In_Cpp_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Unexpected_Int_Divide_By_Zero_Result_History_Package is new Printable_History (Packed_I32.T, Packed_I32.Representation.Image);
   package Fp_Dividing_By_Zero_In_Cpp_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Unexpected_Fp_Divide_By_Zero_Result_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Invalid_Magic_Number_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
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
      Unexpected_Raising_Exception_In_Cpp_Result_History : Unexpected_Raising_Exception_In_Cpp_Result_History_Package.Instance;
      Int_Dividing_By_Zero_In_Cpp_History : Int_Dividing_By_Zero_In_Cpp_History_Package.Instance;
      Unexpected_Int_Divide_By_Zero_Result_History : Unexpected_Int_Divide_By_Zero_Result_History_Package.Instance;
      Fp_Dividing_By_Zero_In_Cpp_History : Fp_Dividing_By_Zero_In_Cpp_History_Package.Instance;
      Unexpected_Fp_Divide_By_Zero_Result_History : Unexpected_Fp_Divide_By_Zero_Result_History_Package.Instance;
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
   overriding procedure Unexpected_Raising_Exception_In_Cpp_Result (Self : in out Instance);
   -- An Int_Divide_By_Zero_In_Cpp command was received and the magic number was
   -- correct. The division will occur in N milliseconds, where N is provided as the
   -- event parameter.
   overriding procedure Int_Dividing_By_Zero_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T);
   -- The integer divide-by-zero in C++ returned a value that did not trigger a
   -- Constraint_Error. This event should never fire under normal operation and
   -- indicates the target does not behave as expected. The parameter is the raw
   -- result returned by C++.
   overriding procedure Unexpected_Int_Divide_By_Zero_Result (Self : in out Instance; Arg : in Packed_I32.T);
   -- An FP_Divide_By_Zero_In_Cpp command was received and the magic number was
   -- correct. The floating-point division will occur in N milliseconds, where N is
   -- provided as the event parameter.
   overriding procedure Fp_Dividing_By_Zero_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T);
   -- The floating-point divide-by-zero in C++ returned a value that did not trigger
   -- a Constraint_Error. This event should never fire under normal operation and
   -- indicates the target does not conform to the C++ reference for IEEE floating-
   -- point division by zero. The parameter is the raw result returned by C++.
   overriding procedure Unexpected_Fp_Divide_By_Zero_Result (Self : in out Instance; Arg : in Packed_F32.T);
   -- A command was received, but the magic number was incorrect.
   overriding procedure Invalid_Magic_Number (Self : in out Instance; Arg : in Packed_U32.T);
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
   -- reflected in the documentation and ground system definitions.
   overriding procedure Last_Chance_Handler_Packet (Self : in out Instance; Arg : in Packed_Exception_Occurrence.T);

end Component.Zero_Divider_Cpp.Implementation.Tester;
