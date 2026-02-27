--------------------------------------------------------------------------------
-- Fault_Component Component Tester Body
--------------------------------------------------------------------------------

package body Component.Fault_Component.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Fault_T_Recv_Sync_History.Init (Depth => 100);
      -- Fault histories:
      Self.Discontinuous_Time_Fault_History.Init (Depth => 100);
      Self.Zero_Time_Fault_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Fault_T_Recv_Sync_History.Destroy;
      -- Fault histories:
      Self.Discontinuous_Time_Fault_History.Destroy;
      Self.Zero_Time_Fault_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Fault_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Fault_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to send out a fault.
   overriding procedure Fault_T_Recv_Sync (Self : in out Instance; Arg : in Fault.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the fault to the correct handler:
      Self.Dispatch_Fault (Arg);
   end Fault_T_Recv_Sync;

   -----------------------------------------------
   -- Fault handler primitive:
   -----------------------------------------------
   -- Description:
   --    A set of faults for the Fault Component.
   -- A discontinuous time was detected by the component.
   overriding procedure Discontinuous_Time_Fault (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Discontinuous_Time_Fault_History.Push (Arg);
   end Discontinuous_Time_Fault;

   -- A time restart at zero seconds epoch was detected by the component.
   overriding procedure Zero_Time_Fault (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Zero_Time_Fault_History.Push (Arg);
   end Zero_Time_Fault;

end Component.Fault_Component.Implementation.Tester;
