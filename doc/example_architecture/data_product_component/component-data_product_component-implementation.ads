--------------------------------------------------------------------------------
-- Data_Product_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Interfaces; use Interfaces;

-- This is the data product component, which sends data products.
package Component.Data_Product_Component.Implementation is

   -- The component class instance record:
   type Instance is new Data_Product_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Data_Product_Component.Base_Instance with record
      Count : Unsigned_16 := 0;
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
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

end Component.Data_Product_Component.Implementation;
