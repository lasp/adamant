-- Includes:
with Component.Generic_Component_Reciprocal;
with Sys_Time;
with History;

-- This is a simple test component with generic connectors.
generic
package Component.Generic_Component.Implementation.Tester is

   package Generic_Component_Package is new Component.Generic_Component_Reciprocal (Generic_Type_1, Generic_Type_2);
   -- Invoker connector history packages:
   package Generic_Type_1_Recv_Sync_History_Package is new History (Generic_Type_1);

   -- Component class instance:
   type Instance is new Generic_Component_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Generic_Component.Implementation.Instance;
      -- System time for test:
      System_Time : Sys_Time.T := (0, 0);
      -- Connector histories:
      Generic_Type_1_Recv_Sync_History : Generic_Type_1_Recv_Sync_History_Package.Instance;
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
   -- The output connector
   overriding procedure Generic_Type_1_Recv_Sync (Self : in out Instance; Arg : in Generic_Type_1);

end Component.Generic_Component.Implementation.Tester;
