--------------------------------------------------------------------------------
-- Product_Packetizer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Product_Packetizer_Reciprocal;
with Sys_Time;
with Printable_History;
with Data_Product_Fetch.Representation;
with Data_Product_Return;
with Packet.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Event;
with Invalid_Packet_Id.Representation;
with Packet_Period.Representation;
with Packet_Data_Product_Ids.Representation;
with Invalid_Data_Product_Length.Representation;
with Invalid_Command_Info.Representation;
with Data_Product_Types; use Data_Product_Types;
with Command_Header.Representation;

-- Custom Includes:
with Test_Assembly_Product_Packets_Test_Packets;
with Data_Product_Enums; use Data_Product_Enums;

-- The product packetizer requests data products from an external component and packetizes them into packets at a configurable rate. The packets that this component produces are configured via an autocoded table.
package Component.Product_Packetizer.Implementation.Tester is

   -- Invoker connector history packages:
   package Data_Product_Fetch_T_Service_History_Package is new Printable_History (Data_Product_Fetch.T, Data_Product_Fetch.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);

   -- Event history packages:
   package Invalid_Packet_Id_Commanded_History_Package is new Printable_History (Invalid_Packet_Id.T, Invalid_Packet_Id.Representation.Image);
   package Packet_Enabled_History_Package is new Printable_History (Packet_Period.T, Packet_Period.Representation.Image);
   package Packet_Disabled_History_Package is new Printable_History (Packet_Period.T, Packet_Period.Representation.Image);
   package Packet_Enabled_On_Change_History_Package is new Printable_History (Packet_Period.T, Packet_Period.Representation.Image);
   package Packet_Period_Set_History_Package is new Printable_History (Packet_Period.T, Packet_Period.Representation.Image);
   package Data_Product_Missing_On_Fetch_History_Package is new Printable_History (Packet_Data_Product_Ids.T, Packet_Data_Product_Ids.Representation.Image);
   package Packet_Period_Item_Bad_Id_History_Package is new Printable_History (Packet_Data_Product_Ids.T, Packet_Data_Product_Ids.Representation.Image);
   package Data_Product_Length_Mismatch_History_Package is new Printable_History (Invalid_Data_Product_Length.T, Invalid_Data_Product_Length.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Dropped_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);

   -- Packet history packages:
   package Dummy_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Product_Packetizer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Product_Packetizer.Implementation.Instance (Test_Assembly_Product_Packets_Test_Packets.Packet_List'Access);
      -- Connector histories:
      Data_Product_Fetch_T_Service_History : Data_Product_Fetch_T_Service_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Packet_Id_Commanded_History : Invalid_Packet_Id_Commanded_History_Package.Instance;
      Packet_Enabled_History : Packet_Enabled_History_Package.Instance;
      Packet_Disabled_History : Packet_Disabled_History_Package.Instance;
      Packet_Enabled_On_Change_History : Packet_Enabled_On_Change_History_Package.Instance;
      Packet_Period_Set_History : Packet_Period_Set_History_Package.Instance;
      Data_Product_Missing_On_Fetch_History : Data_Product_Missing_On_Fetch_History_Package.Instance;
      Packet_Period_Item_Bad_Id_History : Packet_Period_Item_Bad_Id_History_Package.Instance;
      Data_Product_Length_Mismatch_History : Data_Product_Length_Mismatch_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Dropped_Command_History : Dropped_Command_History_Package.Instance;
      -- Packet histories:
      Dummy_History : Dummy_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      -- Status for data product return:
      Data_Product_Fetch_Return_Status : Fetch_Status.E := Fetch_Status.Success;
      -- Optional override for returning a specific data product when fetched.
      Use_Data_Product_Return_Value : Boolean := False;
      Data_Product_Return_Target_Id : Data_Product_Id := 0;
      Data_Product_Return_Value : Data_Product_Return.T := (
         The_Status => Fetch_Status.Success,
         The_Data_Product => (
            Header => (Time => (0, 0), Id => 0, Buffer_Length => 0),
            Buffer => [others => 0]
         )
      );
      -- This variable is used to override the data product return length, which can be used to
      -- induce a length mismatch error during testing. A value of zero does NOT override, any
      -- other value does.
      Data_Product_Length_Override : Data_Product_Buffer_Length_Type := 0;
      -- Data product time:
      Dp_Time : Sys_Time.T := (0, 0);
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Fetch a data product item from the database.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T;
   -- Send a packet of data products.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- An invalid packet id was commanded for a given command.
   overriding procedure Invalid_Packet_Id_Commanded (Self : in out Instance; Arg : Invalid_Packet_Id.T);
   -- A packet was enabled.
   overriding procedure Packet_Enabled (Self : in out Instance; Arg : Packet_Period.T);
   -- A packet was disabled.
   overriding procedure Packet_Disabled (Self : in out Instance; Arg : Packet_Period.T);
   -- A packet was enabled in on-change mode.
   overriding procedure Packet_Enabled_On_Change (Self : in out Instance; Arg : Packet_Period.T);
   -- A packet period was set.
   overriding procedure Packet_Period_Set (Self : in out Instance; Arg : Packet_Period.T);
   -- A data product was missing when fetched for packet insertion.
   overriding procedure Data_Product_Missing_On_Fetch (Self : in out Instance; Arg : Packet_Data_Product_Ids.T);
   -- A packet period packet item could not be formed because the ID is invalid.
   overriding procedure Packet_Period_Item_Bad_Id (Self : in out Instance; Arg : in Packet_Data_Product_Ids.T);
   -- A data product was fetched but contained an unexpected length.
   overriding procedure Data_Product_Length_Mismatch (Self : in out Instance; Arg : Invalid_Data_Product_Length.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : Command_Header.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Packetizer. This packet list is populated based on the product packets model provided to the Product Packetizer component at initialization.
   -- A placeholder packet that will be overwritten with the packet list provided by the product packets model.
   overriding procedure Dummy (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : Positive := 1) return Natural;

   -----------------------------------------------
   -- Custom function for resetting component internal count
   -----------------------------------------------
   not overriding procedure Set_Count (Self : in out Instance; Cnt : in Positive);
   not overriding procedure Reset_Count (Self : in out Instance);
   not overriding function Get_Count (Self : in out Instance) return Positive;
   not overriding function Get_Roll_Over (Self : in out Instance) return Positive;

end Component.Product_Packetizer.Implementation.Tester;
