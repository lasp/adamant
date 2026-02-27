--------------------------------------------------------------------------------
-- Product_Copier Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Product_Copier_Reciprocal;
with Printable_History;
with Data_Product.Representation;
with Data_Product_Return.Representation;
with Data_Product_Fetch.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Product_Copier_Error_Info.Representation;

-- Given two locations (e.g. a database) and a list of source/destination IDs
-- ("mappings"), fetches Data_Product entries from one location and copies/sends
-- them to another upon receiving a Tick. One potential use case is to take
-- snapshots of a database at fixed intervals, in order to provide a stable view
-- of it to other components despite a high volume of writes.
package Component.Product_Copier.Implementation.Tester is

   use Component.Product_Copier_Reciprocal;
   -- Invoker connector history packages:
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Data_Product_Fetch_T_Service_History_Package is new Printable_History (Data_Product_Fetch.T, Data_Product_Fetch.Representation.Image);
   package Data_Product_Fetch_T_Service_Return_History_Package is new Printable_History (Data_Product_Return.T, Data_Product_Return.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Source_Not_Available_History_Package is new Printable_History (Product_Copier_Error_Info.T, Product_Copier_Error_Info.Representation.Image);
   package Source_Id_Out_Of_Range_History_Package is new Printable_History (Product_Copier_Error_Info.T, Product_Copier_Error_Info.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Product_Copier_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Product_Copier.Implementation.Instance;
      -- Connector histories:
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Data_Product_Fetch_T_Service_History : Data_Product_Fetch_T_Service_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Source_Not_Available_History : Source_Not_Available_History_Package.Instance;
      Source_Id_Out_Of_Range_History : Source_Id_Out_Of_Range_History_Package.Instance;
      Case_1_Ctr : Integer := 0;
      Case_2_Ctr : Integer := 0;
      Case_3_Ctr : Integer := 0;
      Case_4_Ctr : Integer := 0;
      Case_5_Ctr : Integer := 0;
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
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T;
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A data product fetch resulted in a Not_Available status, and was not read
   -- from the source.
   overriding procedure Source_Not_Available (Self : in out Instance; Arg : in Product_Copier_Error_Info.T);
   -- A data product fetch resulted in an Id_Out_Of_Range status, and was not read
   -- from the source.
   overriding procedure Source_Id_Out_Of_Range (Self : in out Instance; Arg : in Product_Copier_Error_Info.T);

end Component.Product_Copier.Implementation.Tester;
