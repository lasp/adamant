--------------------------------------------------------------------------------
-- Interrupt_Servicer Component Tester Body
--------------------------------------------------------------------------------

package body Component.Interrupt_Servicer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Interrupt_Data_Type_Recv_Sync_History.Init (Depth => 10);
      Self.Sys_Time_T_Return_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Interrupt_Data_Type_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Interrupt_Data_Type_Send (Self'Unchecked_Access, Self.Interrupt_Data_Type_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The tick send connection.
   overriding procedure Interrupt_Data_Type_Recv_Sync (Self : in out Instance; Arg : in Interrupt_Data_Type) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Interrupt_Data_Type_Recv_Sync_History.Push (Arg);
   end Interrupt_Data_Type_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

end Component.Interrupt_Servicer.Implementation.Tester;
