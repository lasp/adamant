--------------------------------------------------------------------------------
-- Connector_Protector Component Implementation Spec
--------------------------------------------------------------------------------

-- This is a generic component that can be used to protect (as in protected object) the call to the input connector. The component serves as a multi-tasking safe synchronization point for multiple callers. When the T_Recv_Sync connector is called, the component immediately calls the T_Send connector, passing through any arguments. The T_Send connector is called from within a protected object, and so its execution is atomic with respect to other callers of T_Recv_Sync. That is, the call to T_Send will finish before another task is allowed to invoke T_Recv_Sync (and thus T_Send). The protection mechanism effectively makes all downstream connector calls of this component thread-safe. The advantage of this component is that deploying it appropriately in an assembly can provide thread-safety to components which are not designed to be thread-safe in and of themselves.
generic
package Component.Connector_Protector.Implementation is

   -- The component class instance record:
   type Instance is new Connector_Protector.Base_Instance with private;

private

   -- Protected object which provides mutual exclusion to connector
   -- invocation.
   protected type Protected_Connector is
      -- Procedures requiring full mutual exclusion:
      procedure Call (Self : in out Instance; Arg : in T);

   private
   -- No data to protect.
   end Protected_Connector;

   -- The component class instance record:
   type Instance is new Connector_Protector.Base_Instance with record
      P_Connector : Protected_Connector;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T) is null;

end Component.Connector_Protector.Implementation;
