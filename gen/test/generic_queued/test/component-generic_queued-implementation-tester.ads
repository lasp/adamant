-- Includes:
with Component.Generic_Queued_Reciprocal;
with Sys_Time;
with History;

-- This is a queued test component with generic connectors.
generic
package Component.Generic_Queued.Implementation.Tester is

   package Generic_Queued_Package is new Component.Generic_Queued_Reciprocal (Generic_Type_1, Generic_Type_2, Serialized_Length);
   -- Invoker connector history packages:
   package Generic_Type_1_Recv_Sync_History_Package is new History (Generic_Type_1);

   -- Component class instance:
   type Instance is new Generic_Queued_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Generic_Queued.Implementation.Instance;
      -- System time for test:
      System_Time : Sys_Time.T := (0, 0);
      -- Connector histories:
      Generic_Type_1_Recv_Sync_History : Generic_Type_1_Recv_Sync_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Generic_Type_2_Send_Dropped : Boolean := False;
      Generic_Type_2_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The output connector
   overriding procedure Generic_Type_1_Recv_Sync (Self : in out Instance; Arg : in Generic_Type_1);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Generic_Type_2_Send message is dropped due to a full queue.
   overriding procedure Generic_Type_2_Send_Dropped (Self : in out Instance; Arg : in Generic_Type_2);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Generic_Queued.Implementation.Tester;
