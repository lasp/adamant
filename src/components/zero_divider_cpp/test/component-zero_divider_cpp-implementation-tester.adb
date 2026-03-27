--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Tester Body
--------------------------------------------------------------------------------

package body Component.Zero_Divider_Cpp.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Raising_Exception_In_Cpp_History.Init (Depth => 100);
      Self.Unexpected_Raising_Exception_In_Cpp_Result_History.Init (Depth => 100);
      Self.Int_Dividing_By_Zero_In_Cpp_History.Init (Depth => 100);
      Self.Unexpected_Int_Divide_By_Zero_Result_History.Init (Depth => 100);
      Self.Fp_Dividing_By_Zero_In_Cpp_History.Init (Depth => 100);
      Self.Unexpected_Fp_Divide_By_Zero_Result_History.Init (Depth => 100);
      Self.Invalid_Magic_Number_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Packet histories:
      Self.Last_Chance_Handler_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Raising_Exception_In_Cpp_History.Destroy;
      Self.Unexpected_Raising_Exception_In_Cpp_Result_History.Destroy;
      Self.Int_Dividing_By_Zero_In_Cpp_History.Destroy;
      Self.Unexpected_Int_Divide_By_Zero_Result_History.Destroy;
      Self.Fp_Dividing_By_Zero_In_Cpp_History.Destroy;
      Self.Unexpected_Fp_Divide_By_Zero_Result_History.Destroy;
      Self.Invalid_Magic_Number_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Packet histories:
      Self.Last_Chance_Handler_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A Raise_Exception_In_Cpp command was received and the magic number was correct.
   -- The exception will be raised in N milliseconds, where N is provided as the
   -- event parameter.
   overriding procedure Raising_Exception_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Raising_Exception_In_Cpp_History.Push (Arg);
   end Raising_Exception_In_Cpp;

   -- The C++ exception raise did not propagate as expected. This event should never
   -- fire under normal operation and indicates the target does not propagate C++
   -- exceptions to Ada as expected.
   overriding procedure Unexpected_Raising_Exception_In_Cpp_Result (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Raising_Exception_In_Cpp_Result_History.Push (Arg);
   end Unexpected_Raising_Exception_In_Cpp_Result;

   -- An Int_Divide_By_Zero_In_Cpp command was received and the magic number was
   -- correct. The division will occur in N milliseconds, where N is provided as the
   -- event parameter.
   overriding procedure Int_Dividing_By_Zero_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Int_Dividing_By_Zero_In_Cpp_History.Push (Arg);
   end Int_Dividing_By_Zero_In_Cpp;

   -- The integer divide-by-zero in C++ returned a value that did not trigger a
   -- Constraint_Error. This event should never fire under normal operation and
   -- indicates the target does not behave as expected. The parameter is the raw
   -- result returned by C++.
   overriding procedure Unexpected_Int_Divide_By_Zero_Result (Self : in out Instance; Arg : in Packed_I32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Int_Divide_By_Zero_Result_History.Push (Arg);
   end Unexpected_Int_Divide_By_Zero_Result;

   -- An FP_Divide_By_Zero_In_Cpp command was received and the magic number was
   -- correct. The floating-point division will occur in N milliseconds, where N is
   -- provided as the event parameter.
   overriding procedure Fp_Dividing_By_Zero_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fp_Dividing_By_Zero_In_Cpp_History.Push (Arg);
   end Fp_Dividing_By_Zero_In_Cpp;

   -- The floating-point divide-by-zero in C++ returned a value that did not trigger
   -- a Constraint_Error. This event should never fire under normal operation and
   -- indicates the target does not conform to the C++ reference for IEEE floating-
   -- point division by zero. The parameter is the raw result returned by C++.
   overriding procedure Unexpected_Fp_Divide_By_Zero_Result (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Fp_Divide_By_Zero_Result_History.Push (Arg);
   end Unexpected_Fp_Divide_By_Zero_Result;

   -- A command was received, but the magic number was incorrect.
   overriding procedure Invalid_Magic_Number (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Magic_Number_History.Push (Arg);
   end Invalid_Magic_Number;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -----------------------------------------------
   -- Packet handler primitive:
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
   overriding procedure Last_Chance_Handler_Packet (Self : in out Instance; Arg : in Packed_Exception_Occurrence.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Chance_Handler_Packet_History.Push (Arg);
   end Last_Chance_Handler_Packet;

end Component.Zero_Divider_Cpp.Implementation.Tester;
