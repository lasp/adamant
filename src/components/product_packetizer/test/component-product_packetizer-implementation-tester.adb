--------------------------------------------------------------------------------
-- Product_Packetizer Component Tester Body
--------------------------------------------------------------------------------

with Packed_U32;
with Data_Product;
with Packed_U16;
with String_Util;

package body Component.Product_Packetizer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Data_Product_Fetch_T_Service_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Packet_Id_Commanded_History.Init (Depth => 100);
      Self.Packet_Enabled_History.Init (Depth => 100);
      Self.Packet_Disabled_History.Init (Depth => 100);
      Self.Packet_Enabled_On_Change_History.Init (Depth => 100);
      Self.Packet_Period_Set_History.Init (Depth => 100);
      Self.Data_Product_Missing_On_Fetch_History.Init (Depth => 100);
      Self.Packet_Period_Item_Bad_Id_History.Init (Depth => 100);
      Self.Data_Product_Length_Mismatch_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Dropped_Command_History.Init (Depth => 100);
      -- Packet histories:
      Self.Dummy_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Data_Product_Fetch_T_Service_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Invalid_Packet_Id_Commanded_History.Destroy;
      Self.Packet_Enabled_History.Destroy;
      Self.Packet_Disabled_History.Destroy;
      Self.Packet_Enabled_On_Change_History.Destroy;
      Self.Packet_Period_Set_History.Destroy;
      Self.Data_Product_Missing_On_Fetch_History.Destroy;
      Self.Packet_Period_Item_Bad_Id_History.Destroy;
      Self.Data_Product_Length_Mismatch_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Dropped_Command_History.Destroy;
      -- Packet histories:
      Self.Dummy_History.Destroy;
      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Data_Product_Fetch_T_Request (Self'Unchecked_Access, Self.Data_Product_Fetch_T_Service_Access);
      Self.Component_Instance.Attach_Packet_T_Send (Self'Unchecked_Access, Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (Self'Unchecked_Access, Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (Self'Unchecked_Access, Self.Command_Response_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Fetch a data product item from the database.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T is
      To_Return : Data_Product_Return.T;
      Dp : Data_Product.T;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Fetch_T_Service_History.Push (Arg);

      -- We need to simulate the return of an actual data product here:
      Dp.Header.Time := Self.Dp_Time;
      case Arg.Id is
         -- A, U32
         when 1 =>
            Dp.Header.Id := 0;
            Dp.Header.Buffer_Length := Packed_U32.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Packed_U32.Serialization.To_Byte_Array ((Value => 23));
            -- B, Tick.T
         when 2 =>
            Dp.Header.Id := 1;
            Dp.Header.Buffer_Length := Tick.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Tick.Serialization.To_Byte_Array ((Dp.Header.Time, 13));
            -- C, Tick.T
         when 3 =>
            Dp.Header.Id := 2;
            Dp.Header.Buffer_Length := Tick.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Tick.Serialization.To_Byte_Array ((Dp.Header.Time, 14));
            -- D, U16
         when 4 =>
            Dp.Header.Id := 3;
            Dp.Header.Buffer_Length := Packed_U16.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => 33));
         when others =>
            pragma Assert (False, "Unexpected id received.");
      end case;

      -- Override the length if necessary:
      if Self.Data_Product_Length_Override > 0 then
         Dp.Header.Buffer_Length := Self.Data_Product_Length_Override;
      end if;

      To_Return.The_Status := Self.Data_Product_Fetch_Return_Status;
      To_Return.The_Data_Product := Dp;
      return To_Return;
   end Data_Product_Fetch_T_Service;

   -- Send a packet of data products.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
   end Packet_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to register the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_Send was called!");
      else
         Self.Command_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_Send_Dropped := False;
      end if;
   end Command_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- An invalid packet id was commanded for a given command.
   overriding procedure Invalid_Packet_Id_Commanded (Self : in out Instance; Arg : Invalid_Packet_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Packet_Id_Commanded_History.Push (Arg);
   end Invalid_Packet_Id_Commanded;

   -- A packet was enabled.
   overriding procedure Packet_Enabled (Self : in out Instance; Arg : Packet_Period.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Enabled_History.Push (Arg);
   end Packet_Enabled;

   -- A packet was disabled.
   overriding procedure Packet_Disabled (Self : in out Instance; Arg : Packet_Period.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Disabled_History.Push (Arg);
   end Packet_Disabled;

   -- A packet was enabled in on-change mode.
   overriding procedure Packet_Enabled_On_Change (Self : in out Instance; Arg : in Packet_Period.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Enabled_On_Change_History.Push (Arg);
   end Packet_Enabled_On_Change;

   -- A packet period was set.
   overriding procedure Packet_Period_Set (Self : in out Instance; Arg : Packet_Period.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Period_Set_History.Push (Arg);
   end Packet_Period_Set;

   -- A data product was missing when fetched for packet insertion.
   overriding procedure Data_Product_Missing_On_Fetch (Self : in out Instance; Arg : Packet_Data_Product_Ids.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Missing_On_Fetch_History.Push (Arg);
   end Data_Product_Missing_On_Fetch;

   -- A packet period packet item could not be formed because the ID is invalid.
   overriding procedure Packet_Period_Item_Bad_Id (Self : in out Instance; Arg : in Packet_Data_Product_Ids.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Period_Item_Bad_Id_History.Push (Arg);
   end Packet_Period_Item_Bad_Id;

   -- A data product was fetched but contained an unexpected length.
   overriding procedure Data_Product_Length_Mismatch (Self : in out Instance; Arg : Invalid_Data_Product_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Length_Mismatch_History.Push (Arg);
   end Data_Product_Length_Mismatch;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_History.Push (Arg);
   end Dropped_Command;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Packetizer. This packet list is populated based on the product packets model provided to the Product Packetizer component at initialization.
   -- A placeholder packet that will be overwritten with the packet list provided by the product packets model.
   overriding procedure Dummy (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dummy_History.Push (Arg);
   end Dummy;

   -----------------------------------------------
   -- Special primitives for activating queued
   -- components:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

   -----------------------------------------------
   -- Custom function for resetting component internal count
   -----------------------------------------------

   not overriding procedure Set_Count (Self : in out Instance; Cnt : in Positive) is
   begin
      Self.Component_Instance.Count := Cnt;
   end Set_Count;

   not overriding procedure Reset_Count (Self : in out Instance) is
   begin
      Self.Set_Count (1);
   end Reset_Count;

   not overriding function Get_Count (Self : in out Instance) return Positive is
   begin
      return Self.Component_Instance.Count;
   end Get_Count;

   not overriding function Get_Roll_Over (Self : in out Instance) return Positive is
   begin
      return Self.Component_Instance.Roll_Over_Value;
   end Get_Roll_Over;

end Component.Product_Packetizer.Implementation.Tester;
