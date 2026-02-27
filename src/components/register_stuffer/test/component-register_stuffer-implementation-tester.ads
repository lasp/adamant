--------------------------------------------------------------------------------
-- Register_Stuffer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Register_Stuffer_Reciprocal;
with Printable_History;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Packet.Representation;
with Register_Dump_Packet.Representation;
with Event;
with Packed_Address.Representation;
with Register_Value.Representation;
with Invalid_Command_Info.Representation;
with Packed_Arm_Timeout.Representation;
with Register_Dump_Packet_Header.Representation;
with Data_Product;
with Packed_Arm_State.Representation;

-- This component services commands to stuff and dump registers. This component
-- is different than the memory stuffer/dumper in that it atomically sets 32-bit
-- little endian registers, which is a requirement on some hardware. It rejects
-- commands to stuff or dump addresses that are not on a 4-byte boundary. Note
-- that this component assumes all registers it accesses are little endian.
-- Another version of this component needs to be used to access registers that
-- are big endian.
package Component.Register_Stuffer.Implementation.Tester is

   use Component.Register_Stuffer_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Event history packages:
   package Invalid_Register_Address_History_Package is new Printable_History (Packed_Address.T, Packed_Address.Representation.Image);
   package Register_Written_History_Package is new Printable_History (Register_Value.T, Register_Value.Representation.Image);
   package Register_Read_History_Package is new Printable_History (Register_Value.T, Register_Value.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Rejected_Protected_Register_Write_History_Package is new Printable_History (Register_Value.T, Register_Value.Representation.Image);
   package Armed_History_Package is new Printable_History (Packed_Arm_Timeout.T, Packed_Arm_Timeout.Representation.Image);
   package Unarmed_History_Package is new Printable_History (Natural, Natural'Image);
   package Unarmed_Timeout_History_Package is new Printable_History (Natural, Natural'Image);
   package Registers_Dumped_History_Package is new Printable_History (Register_Dump_Packet_Header.T, Register_Dump_Packet_Header.Representation.Image);
   package Address_Range_Overflow_History_Package is new Printable_History (Register_Dump_Packet_Header.T, Register_Dump_Packet_Header.Representation.Image);

   -- Data product history packages:
   package Last_Register_Written_History_Package is new Printable_History (Register_Value.T, Register_Value.Representation.Image);
   package Last_Register_Read_History_Package is new Printable_History (Register_Value.T, Register_Value.Representation.Image);
   package Armed_State_History_Package is new Printable_History (Packed_Arm_State.T, Packed_Arm_State.Representation.Image);
   package Armed_State_Timeout_History_Package is new Printable_History (Packed_Arm_Timeout.T, Packed_Arm_Timeout.Representation.Image);

   -- Packet history packages:
   package Register_Packet_History_Package is new Printable_History (Register_Dump_Packet.T, Register_Dump_Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Register_Stuffer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Register_Stuffer.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Register_Address_History : Invalid_Register_Address_History_Package.Instance;
      Register_Written_History : Register_Written_History_Package.Instance;
      Register_Read_History : Register_Read_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Rejected_Protected_Register_Write_History : Rejected_Protected_Register_Write_History_Package.Instance;
      Armed_History : Armed_History_Package.Instance;
      Unarmed_History : Unarmed_History_Package.Instance;
      Unarmed_Timeout_History : Unarmed_Timeout_History_Package.Instance;
      Registers_Dumped_History : Registers_Dumped_History_Package.Instance;
      Address_Range_Overflow_History : Address_Range_Overflow_History_Package.Instance;
      -- Data product histories:
      Last_Register_Written_History : Last_Register_Written_History_Package.Instance;
      Last_Register_Read_History : Last_Register_Read_History_Package.Instance;
      Armed_State_History : Armed_State_History_Package.Instance;
      Armed_State_Timeout_History : Armed_State_Timeout_History_Package.Instance;
      -- Packet histories:
      Register_Packet_History : Register_Packet_History_Package.Instance;
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
   -- This connector is used to send the command response back to the command router.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The event send connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- Packets are sent out of this connector
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Register Stuffer component.
   -- The register address provided does not start on a 32-bit boundary.
   overriding procedure Invalid_Register_Address (Self : in out Instance; Arg : in Packed_Address.T);
   -- The specified register was written to the commanded value.
   overriding procedure Register_Written (Self : in out Instance; Arg : in Register_Value.T);
   -- The specified register was read from.
   overriding procedure Register_Read (Self : in out Instance; Arg : in Register_Value.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- The specified register could not be written because the component was not armed
   -- first.
   overriding procedure Rejected_Protected_Register_Write (Self : in out Instance; Arg : in Register_Value.T);
   -- The component received the arm command and is now armed.
   overriding procedure Armed (Self : in out Instance; Arg : in Packed_Arm_Timeout.T);
   -- The component received a command and is now unarmed.
   overriding procedure Unarmed (Self : in out Instance);
   -- The component armed state timed out and is now unarmed.
   overriding procedure Unarmed_Timeout (Self : in out Instance);
   -- The specified registers were dumped.
   overriding procedure Registers_Dumped (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T);
   -- The specified registers were dumped.
   overriding procedure Address_Range_Overflow (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Register Stuffer component.
   -- The address and value of the last written register.
   overriding procedure Last_Register_Written (Self : in out Instance; Arg : in Register_Value.T);
   -- The address and value of the last read register.
   overriding procedure Last_Register_Read (Self : in out Instance; Arg : in Register_Value.T);
   -- The current armed/unarmed state of the component.
   overriding procedure Armed_State (Self : in out Instance; Arg : in Packed_Arm_State.T);
   -- The time remaining (in ticks) until the armed state expires.
   overriding procedure Armed_State_Timeout (Self : in out Instance; Arg : in Packed_Arm_Timeout.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the register stuffer.
   -- This packet contains dumped register values.
   overriding procedure Register_Packet (Self : in out Instance; Arg : in Register_Dump_Packet.T);

end Component.Register_Stuffer.Implementation.Tester;
