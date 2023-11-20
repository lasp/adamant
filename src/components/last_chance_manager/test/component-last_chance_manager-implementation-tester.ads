--------------------------------------------------------------------------------
-- Last_Chance_Manager Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Last_Chance_Manager_Reciprocal;
with Printable_History;
with Command_Response.Representation;
with Sys_Time.Representation;
with Event.Representation;
with Packet.Representation;
with Data_Product.Representation;
with Event;
with Packed_Stack_Trace_Info.Representation;
with Invalid_Command_Info.Representation;
with Packed_Exception_Occurrence.Representation;
with Data_Product;

-- The purpose of this component is to manage a region of non-volatile memory where the last chance handler saves exception information, should one be thrown. This component provides commands to dump this region of memory and reset the contents of the memory to all zeros. The component provides a data product that reports the first address of the stack trace, which can be used as confirmation that the LCH got called (if the value is nonzero).
package Component.Last_Chance_Manager.Implementation.Tester is

   use Component.Last_Chance_Manager_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);

   -- Event history packages:
   package Last_Chance_Handler_Called_History_Package is new Printable_History (Packed_Stack_Trace_Info.T, Packed_Stack_Trace_Info.Representation.Image);
   package Dumped_Last_Chance_Handler_Region_History_Package is new Printable_History (Natural, Natural'Image);
   package Cleared_Last_Chance_Handler_Region_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Lch_Stack_Trace_Info_History_Package is new Printable_History (Packed_Stack_Trace_Info.T, Packed_Stack_Trace_Info.Representation.Image);

   -- Packet history packages:
   package Lch_Memory_Region_Dump_History_Package is new Printable_History (Packed_Exception_Occurrence.T, Packed_Exception_Occurrence.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Last_Chance_Manager_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Last_Chance_Manager.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Last_Chance_Handler_Called_History : Last_Chance_Handler_Called_History_Package.Instance;
      Dumped_Last_Chance_Handler_Region_History : Dumped_Last_Chance_Handler_Region_History_Package.Instance;
      Cleared_Last_Chance_Handler_Region_History : Cleared_Last_Chance_Handler_Region_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Lch_Stack_Trace_Info_History : Lch_Stack_Trace_Info_History_Package.Instance;
      -- Packet histories:
      Lch_Memory_Region_Dump_History : Lch_Memory_Region_Dump_History_Package.Instance;
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
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Send a packet of data products.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The component detected that the LCH was called by looking at the data in nonvolatile memory. The lowest level address of the stack trace is reported.
   overriding procedure Last_Chance_Handler_Called (Self : in out Instance; Arg : in Packed_Stack_Trace_Info.T);
   -- The component dumped the last chance handler memory region into a packet for downlink.
   overriding procedure Dumped_Last_Chance_Handler_Region (Self : in out Instance);
   -- The component cleared the last chance handler memory region by writing all zeros to it.
   overriding procedure Cleared_Last_Chance_Handler_Region (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Last Chance Manager component.
   -- Information on the current stack trace stored in the last chance handler memory store.
   overriding procedure Lch_Stack_Trace_Info (Self : in out Instance; Arg : in Packed_Stack_Trace_Info.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    The second packet listed here is not actually produced by the Last Chance Manager component, but instead should be produced by the implementation of the Last\_Chance\_Handler. This packet definition exists to ensure that the packet gets reflected in the documentation and ground system definitions.
   -- This packet contains a dump of the LCH nonvolatile memory region where exception information is thrown.
   overriding procedure Lch_Memory_Region_Dump (Self : in out Instance; Arg : in Packed_Exception_Occurrence.T);

end Component.Last_Chance_Manager.Implementation.Tester;
