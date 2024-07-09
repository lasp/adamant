--------------------------------------------------------------------------------
-- Limiter Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with Parameter;
with String_Util;

package body Component.Limiter.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Max_Send_Per_Tick_Set_History.Init (Depth => 100);
      Self.Data_Dropped_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Invalid_Parameter_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Max_Packet_Sends_Per_Tick_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Max_Send_Per_Tick_Set_History.Destroy;
      Self.Data_Dropped_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Invalid_Parameter_Received_History.Destroy;
      -- Data product histories:
      Self.Max_Packet_Sends_Per_Tick_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.T_Recv_Async_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
      Self.Attach_Parameter_Update_T_Provide (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Parameter_Update_T_Modify_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invoker connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.T_Recv_Sync_History.Push (Arg);
   end T_Recv_Sync;

   -- This connector is used to register and respond to the component's commands. This does not need to be connected if the command for this component will not be used.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

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

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T) is
      Ignore : T renames Arg;
   begin
      if not Self.Expect_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when T_Send was called!");
      else
         Self.T_Send_Dropped_Count := Self.T_Send_Dropped_Count + 1;
         Self.Expect_T_Send_Dropped := False;
      end if;
   end T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A new maximum sends per tick rate has been set for the limiter.
   overriding procedure Max_Send_Per_Tick_Set (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Send_Per_Tick_Set_History.Push (Arg);
   end Max_Send_Per_Tick_Set;

   -- The queue for data overflowed and an incoming data was dropped.
   overriding procedure Data_Dropped (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Dropped_History.Push (Arg);
   end Data_Dropped;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A parameter was received with invalid values.
   overriding procedure Invalid_Parameter_Received (Self : in out Instance; Arg : in Invalid_Parameter_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Parameter_Received_History.Push (Arg);
   end Invalid_Parameter_Received;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Limiter component.
   -- The current maximum packet sends per tick.
   overriding procedure Max_Packet_Sends_Per_Tick (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Packet_Sends_Per_Tick_History.Push (Arg);
   end Max_Packet_Sends_Per_Tick;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
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
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (Operation => Stage, Status => Success, Param => Par);
   begin
      Self.Parameter_Update_T_Provide (Param_Update);
      return Param_Update.Status;
   end Stage_Parameter;

   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (Operation => Fetch, Status => Success, Param => (Header => (Id => Id, Buffer_Length => 0), Buffer => [others => 0]));
   begin
      -- Set the ID to fetch:
      Param_Update.Param.Header.Id := Id;
      Self.Parameter_Update_T_Provide (Param_Update);
      Par := Param_Update.Param;
      return Param_Update.Status;
   end Fetch_Parameter;

   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (Operation => Update, Status => Success, Param => ((0, 0), [others => 0]));
   begin
      Self.Parameter_Update_T_Provide (Param_Update);
      return Param_Update.Status;
   end Update_Parameters;

end Component.Limiter.Implementation.Tester;
