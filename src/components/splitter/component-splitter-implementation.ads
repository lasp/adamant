--------------------------------------------------------------------------------
-- Splitter Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- This is a generic component that can be used to split a single connector of any type into an arrayed connector of that type. This can be useful when a component has a single send connector, but you actually need the data to go to many different places simultaneously. In this case, the splitter component can be attached to the send connector and the distribute the data to many downstream components.
generic
package Component.Splitter.Implementation is

   type Instance is new Splitter.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Splitter.Base_Instance with record
      null; -- No internal state
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Index : in T_Send_Index; Arg : in T) is null;

end Component.Splitter.Implementation;
