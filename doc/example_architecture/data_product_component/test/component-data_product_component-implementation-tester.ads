--------------------------------------------------------------------------------
-- Data_Product_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Data_Product_Component_Reciprocal;
with Sys_Time;
with Printable_History;
with Sys_Time.Representation;
with Data_Product.Representation;
with Data_Product;
with Packed_U16.Representation;
with Tick.Representation;

-- This is the data product component, which sends data products.
package Component.Data_Product_Component.Implementation.Tester is

   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);

   -- Data product history packages:
   package Counter_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Last_Tick_Received_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Data_Product_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Data_Product_Component.Implementation.Instance;
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      -- Data product histories:
      Counter_History : Counter_History_Package.Instance;
      Last_Tick_Received_History : Last_Tick_Received_History_Package.Instance;
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
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to send out data products.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    A set of data products for the Data Product Component.
   -- A 16-bit counter.
   overriding procedure Counter (Self : in out Instance; Arg : in Packed_U16.T);
   -- A last tick that was received by the component.
   overriding procedure Last_Tick_Received (Self : in out Instance; Arg : in Tick.T);

end Component.Data_Product_Component.Implementation.Tester;
