--------------------------------------------------------------------------------
-- Active_Component Component Tester Body
--------------------------------------------------------------------------------

package body Component.Active_Component.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Init (Depth => 10);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Packet_T_Send (Self'Unchecked_Access, Self.Packet_T_Recv_Sync_Access);
      Self.Attach_Packed_Byte_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Packed_Byte_T_Recv_Async_Access);
      Self.Attach_Packed_U16_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Packed_U16_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to send out a telemetry packet.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
   end Packet_T_Recv_Sync;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packed_Byte_T_Send message is dropped due to a full queue.
   overriding procedure Packed_Byte_T_Send_Dropped (Self : in out Instance; Arg : in Packed_Byte.T) is
      Ignore : Packed_Byte.T renames Arg;
   begin
      if not Self.Expect_Packed_Byte_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Packed_Byte_T_Send was called!");
      else
         Self.Packed_Byte_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Packed_Byte_T_Send_Dropped := False;
      end if;
   end Packed_Byte_T_Send_Dropped;

   -- This procedure is called when a Packed_U16_T_Send message is dropped due to a full queue.
   overriding procedure Packed_U16_T_Send_Dropped (Self : in out Instance; Arg : in Packed_U16.T) is
      Ignore : Packed_U16.T renames Arg;
   begin
      if not Self.Expect_Packed_U16_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Packed_U16_T_Send was called!");
      else
         Self.Packed_U16_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Packed_U16_T_Send_Dropped := False;
      end if;
   end Packed_U16_T_Send_Dropped;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
   begin
      return Self.Component_Instance.Dispatch_All;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
   begin
      return Self.Component_Instance.Dispatch_N (N);
   end Dispatch_N;

end Component.Active_Component.Implementation.Tester;
