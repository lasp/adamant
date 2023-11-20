--------------------------------------------------------------------------------
-- Data_Dependency_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;

-- This is the data dependency component, which fetches data dependencies.
package Component.Data_Dependency_Component.Implementation is

   -- The component class instance record:
   type Instance is new Data_Dependency_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Data_Dependency_Component.Base_Instance with record
      null; -- TODO
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   -----------------------------------------------
   -- Data dependency primitives:
   -----------------------------------------------
   -- Description:
   --    A set of data dependencies for the Data Dependency Component.
   -- Function which retreives a data dependency.
   -- The default implementation is to simply call the Data_Product_Fetch_T_Request connector. Change the implementation if this component
   -- needs to do something different.
   overriding function Get_Data_Dependency (Self : in out Instance; Id : in Data_Product_Types.Data_Product_Id) return Data_Product_Return.T is (Self.Data_Product_Fetch_T_Request ((Id => Id)));

   -- Invalid data dependency handler. This procedure is called when a data dependency's id or length are found to be invalid:
   overriding procedure Invalid_Data_Dependency (Self : in out Instance; Id : in Data_Product_Types.Data_Product_Id; Ret : in Data_Product_Return.T);

end Component.Data_Dependency_Component.Implementation;
