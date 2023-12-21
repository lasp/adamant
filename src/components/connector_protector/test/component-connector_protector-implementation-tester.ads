--------------------------------------------------------------------------------
-- Connector_Protector Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Connector_Protector_Reciprocal;
with History;

-- This is a generic component that can be used to protect (as in protected object) the call to the input connector. The component serves as a multi-tasking safe synchronization point for multiple callers. When the T_Recv_Sync connector is called, the component immediately calls the T_Send connector, passing through any arguments. The T_Send connector is called from within a protected object, and so its execution is atomic with respect to other callers of T_Recv_Sync. That is, the call to T_Send will finish before another task is allowed to invoke T_Recv_Sync (and thus T_Send). The protection mechanism effectively makes all downstream connector calls of this component thread-safe. The advantage of this component is that deploying it appropriately in an assembly can provide thread-safety to components which are not designed to be thread-safe in and of themselves.
generic
package Component.Connector_Protector.Implementation.Tester is

   package Connector_Protector_Package is new Component.Connector_Protector_Reciprocal (T);
   -- Invoker connector history packages:
   package T_Recv_Sync_History_Package is new History (T);

   -- Component class instance:
   type Instance is new Connector_Protector_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Connector_Protector.Implementation.Instance;
      -- Connector histories:
      T_Recv_Sync_History : T_Recv_Sync_History_Package.Instance;
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
   -- The generic invoker connector. Calls originating from this connector are contained within a protected object and thus downstream operations are atomic and thread-safe.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);

end Component.Connector_Protector.Implementation.Tester;
