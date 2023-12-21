--------------------------------------------------------------------------------
-- Forwarder Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Forwarder_Reciprocal;
with Sys_Time;
with Printable_History;
with History;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Data_Product;
with Packed_Enable_Disable_Type.Representation;
with Event;
with Invalid_Command_Info.Representation;

-- This is a generic component that can be used to forward a single connector of any type. The component that synchronously forwards any type that it receives. It includes commands to enable or disable this forwarding, so can be effectively used as a stream on/off switch.
generic
package Component.Forwarder.Implementation.Tester is

   package Forwarder_Package is new Component.Forwarder_Reciprocal (T);
   -- Invoker connector history packages:
   package T_Recv_Sync_History_Package is new History (T);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Forwarding_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Forwarding_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Forwarding_State_History_Package is new Printable_History (Packed_Enable_Disable_Type.T, Packed_Enable_Disable_Type.Representation.Image);

   -- Component class instance:
   type Instance is new Forwarder_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Forwarder.Implementation.Instance;
      -- Connector histories:
      T_Recv_Sync_History : T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Forwarding_Enabled_History : Forwarding_Enabled_History_Package.Instance;
      Forwarding_Disabled_History : Forwarding_Disabled_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Forwarding_State_History : Forwarding_State_History_Package.Instance;
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
   -- The connector that will forward on unfiltered data.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);
   -- The connector that sends a command response when received.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The event connector to send the events specific to the component.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Data forwarding was enabled by command.
   overriding procedure Forwarding_Enabled (Self : in out Instance);
   -- Data forwarding was disabled by command.
   overriding procedure Forwarding_Disabled (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Forwarder component.
   -- Is data forwarding enabled or disabled?
   overriding procedure Forwarding_State (Self : in out Instance; Arg : in Packed_Enable_Disable_Type.T);

end Component.Forwarder.Implementation.Tester;
