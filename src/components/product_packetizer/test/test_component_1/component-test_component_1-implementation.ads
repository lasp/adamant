--------------------------------------------------------------------------------
-- Test_Component_1 Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Tick;

-- A simple component which produces data products for testing the data product packetizer component.
package Component.Test_Component_1.Implementation is

   -- The component class instance record:
   type Instance is new Test_Component_1.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Test_Component_1.Base_Instance with record
      null;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick to regulate the execution of the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

end Component.Test_Component_1.Implementation;
