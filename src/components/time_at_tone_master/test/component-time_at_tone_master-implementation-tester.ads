--------------------------------------------------------------------------------
-- Time_At_Tone_Master Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Time_At_Tone_Master_Reciprocal;
with Sys_Time;
with Printable_History;
with Tick.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Event;
with Invalid_Command_Info.Representation;
with Data_Product;
with Packed_U32.Representation;
with Tat_State.Representation;

-- This is the Time at Tone Master component. TaT is a protocol used to sync a slave clock to a master clock. Two messages are sent from the master to the slave component. First a 'time at tone' message is sent which provides the slave clock with the time that should be stuffed to its clock when the tone message is received. Then a tone message is sent at the appropriate time and the slave clock is updated. This component implements the master side of the protocol. This component outputs the time message and the tone as Tick.T send connectors. This design is intended to be generic enough to implement time at tone in many different manners on the other end of these connectors. For instance, you could convert the time message Tick.T to a CCSDS packet and the tone Tick.T to a GPIO pulse.
package Component.Time_At_Tone_Master.Implementation.Tester is

   use Component.Time_At_Tone_Master_Reciprocal;
   -- Invoker connector history packages:
   package Time_Message_Recv_Sync_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);
   package Tone_Message_Recv_Sync_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Time_At_Tone_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Time_At_Tone_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Sending_Sync_Once_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Tone_Messages_Sent_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Time_At_Tone_State_History_Package is new Printable_History (Tat_State.T, Tat_State.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Time_At_Tone_Master_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Time_At_Tone_Master.Implementation.Instance;
      -- Connector histories:
      Time_Message_Recv_Sync_History : Time_Message_Recv_Sync_History_Package.Instance;
      Tone_Message_Recv_Sync_History : Tone_Message_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Time_At_Tone_Enabled_History : Time_At_Tone_Enabled_History_Package.Instance;
      Time_At_Tone_Disabled_History : Time_At_Tone_Disabled_History_Package.Instance;
      Sending_Sync_Once_History : Sending_Sync_Once_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Tone_Messages_Sent_History : Tone_Messages_Sent_History_Package.Instance;
      Time_At_Tone_State_History : Time_At_Tone_State_History_Package.Instance;
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
   -- Time message send connector, sends a message with the time the tone message will be sent.
   overriding procedure Time_Message_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Tone message send connector.
   overriding procedure Tone_Message_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Used to get system time, used by the master version of the component to get the current time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The data product invoker connector
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The event send connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Time at Tone Master component.
   -- The time at tone has been enabled by command.
   overriding procedure Time_At_Tone_Enabled (Self : in out Instance);
   -- The time at tone has been disabled by command.
   overriding procedure Time_At_Tone_Disabled (Self : in out Instance);
   -- The component will send the time at tone message and tone message at the next received tick.
   overriding procedure Sending_Sync_Once (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Time at Tone Master component.
   -- The number of tone messages sent.
   overriding procedure Tone_Messages_Sent (Self : in out Instance; Arg : in Packed_U32.T);
   -- The disable/enable state of the time at tone component.
   overriding procedure Time_At_Tone_State (Self : in out Instance; Arg : in Tat_State.T);

end Component.Time_At_Tone_Master.Implementation.Tester;
