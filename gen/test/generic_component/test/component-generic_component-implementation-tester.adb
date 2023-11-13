package body Component.Generic_Component.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Generic_Type_1_Recv_Sync_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Generic_Type_1_Recv_Sync_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Generic_Type_1_Send (To_Component => Self'Unchecked_Access, Hook => Self.Generic_Type_1_Recv_Sync_Access);
      Self.Attach_Generic_Type_2_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Generic_Type_2_Recv_Sync_Access);
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

end Component.Generic_Component.Implementation.Tester;
