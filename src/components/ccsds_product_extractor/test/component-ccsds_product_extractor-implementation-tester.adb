--------------------------------------------------------------------------------
-- Ccsds_Product_Extractor Component Tester Body
--------------------------------------------------------------------------------

package body Component.Ccsds_Product_Extractor.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Extracted_Product_Data_History.Init (Depth => 100);
      Self.Invalid_Extracted_Product_Length_History.Init (Depth => 100);
      -- Data product histories:
      Self.Dummy_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Invalid_Extracted_Product_Data_History.Destroy;
      Self.Invalid_Extracted_Product_Length_History.Destroy;
      -- Data product histories:
      Self.Dummy_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The Event connector for sending events
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Event that is issued when the defined extracted product does not match the data that was read.
   overriding procedure Invalid_Extracted_Product_Data (Self : in out Instance; Arg : in Invalid_Product_Data.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Extracted_Product_Data_History.Push (Arg);
   end Invalid_Extracted_Product_Data;

   -- The length and offset of the extracted product exceeded the length of the incoming packet.
   overriding procedure Invalid_Extracted_Product_Length (Self : in out Instance; Arg : in Invalid_Product_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Extracted_Product_Length_History.Push (Arg);
   end Invalid_Extracted_Product_Length;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Ccsds Product Extractor component.
   -- A dummy data product since this component doesnt have its own data products, this provides a base to start from. This will be removed and replaced with the extracted products that the user defines in the extracted_products YAML file.
   overriding procedure Dummy (Self : in out Instance; Arg : in Packed_Byte.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dummy_History.Push (Arg);
   end Dummy;

   -----------------------------------------------
   -- Custom functions
   -----------------------------------------------

   overriding procedure Dispatch_Data_Product (Self : in out Instance; Dp : in Data_Product.T) is
      -- Dispatch to dummy no matter what.
      Dispatch_To : constant Dispatch_Data_Product_Procedure := Data_Product_Id_Table (Ccsds_Product_Extractor_Data_Products.Local_Data_Product_Id_Type'First);
   begin
      Dispatch_To (Component.Ccsds_Product_Extractor_Reciprocal.Base_Instance (Self), Dp);
   end Dispatch_Data_Product;

end Component.Ccsds_Product_Extractor.Implementation.Tester;
