package body Component.Generic_Queued.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Generic_Type_1_Recv_Sync_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Generic_Type_1_Recv_Sync_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Generic_Type_1_Send (To_Component => Self'Unchecked_Access, Hook => Self.Generic_Type_1_Recv_Sync_Access);
      Self.Attach_Generic_Type_2_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Generic_Type_2_Recv_Async_Access);
      Self.Attach_Aa_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Aa_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The output connector
   overriding procedure Generic_Type_1_Recv_Sync (Self : in out Instance; Arg : in Generic_Type_1) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Generic_Type_1_Recv_Sync_History.Push (Arg);
   end Generic_Type_1_Recv_Sync;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Generic_Type_2_Send message is dropped due to a full queue.
   overriding procedure Generic_Type_2_Send_Dropped (Self : in out Instance; Arg : in Generic_Type_2) is
      Ignore : Generic_Type_2 renames Arg;
   begin
      if not Self.Expect_Generic_Type_2_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Generic_Type_2_Send was called!");
      else
         Self.Generic_Type_2_Send_Dropped_Count := @ + 1;
         Self.Expect_Generic_Type_2_Send_Dropped := False;
      end if;
   end Generic_Type_2_Send_Dropped;

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

end Component.Generic_Queued.Implementation.Tester;
