--------------------------------------------------------------------------------
-- Limiter Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Limiter_Reciprocal;
with Sys_Time;
with Printable_History;
with History;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Packed_U16.Representation;
with Invalid_Command_Info.Representation;
with Invalid_Parameter_Info.Representation;
with Data_Product;

-- This is the limiter component. This component receives a generic type of data and queues that data. It then meters the output of the data through a ``send" connector at a commandable rate. The rate is set upon initialization, can be changed by command, or by parameter. The command or parameter connections may be ommitted if one or both of these features are not used. The packet rate is in units of the periodic tick rate that drives the component.
generic
package Component.Limiter.Implementation.Tester is

   package Limiter_Package is new Component.Limiter_Reciprocal (T, Serialized_Length);
   -- Invoker connector history packages:
   package T_Recv_Sync_History_Package is new History (T);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Max_Send_Per_Tick_Set_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Data_Dropped_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Invalid_Parameter_Received_History_Package is new Printable_History (Invalid_Parameter_Info.T, Invalid_Parameter_Info.Representation.Image);

   -- Data product history packages:
   package Max_Packet_Sends_Per_Tick_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Component class instance:
   type Instance is new Limiter_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Limiter.Implementation.Instance;
      -- Connector histories:
      T_Recv_Sync_History : T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Max_Send_Per_Tick_Set_History : Max_Send_Per_Tick_Set_History_Package.Instance;
      Data_Dropped_History : Data_Dropped_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Invalid_Parameter_Received_History : Invalid_Parameter_Received_History_Package.Instance;
      -- Data product histories:
      Max_Packet_Sends_Per_Tick_History : Max_Packet_Sends_Per_Tick_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_T_Send_Dropped : Boolean := False;
      T_Send_Dropped_Count : Natural := 0;
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
   -- The generic invoker connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);
   -- This connector is used to register and respond to the component's commands. This does not need to be connected if the command for this component will not be used.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A new maximum sends per tick rate has been set for the limiter.
   overriding procedure Max_Send_Per_Tick_Set (Self : in out Instance; Arg : in Packed_U16.T);
   -- The queue for data overflowed and an incoming data was dropped.
   overriding procedure Data_Dropped (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A parameter was received with invalid values.
   overriding procedure Invalid_Parameter_Received (Self : in out Instance; Arg : in Invalid_Parameter_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Limiter component.
   -- The current maximum packet sends per tick.
   overriding procedure Max_Packet_Sends_Per_Tick (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

   -----------------------------------------------
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   -- Stage a parameter value within the component
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E;
   -- Fetch the value of a parameter with the component
   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E;
   -- Tell the component it is OK to atomically update all of its
   -- working parameter values with the staged values.
   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E;

end Component.Limiter.Implementation.Tester;
