--------------------------------------------------------------------------------
-- Time_At_Tone_Master Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Command;
with Protected_Variables;
with Ada.Real_Time;
with Interfaces;

-- This is the Time at Tone Master component. TaT is a protocol used to sync a slave clock to a master clock. Two messages are sent from the master to the slave component. First a 'time at tone' message is sent which provides the slave clock with the time that should be stuffed to its clock when the tone message is received. Then a tone message is sent at the appropriate time and the slave clock is updated. This component implements the master side of the protocol. This component outputs the time message and the tone as Tick.T send connectors. This design is intended to be generic enough to implement time at tone in many different manners on the other end of these connectors. For instance, you could convert the time message Tick.T to a CCSDS packet and the tone Tick.T to a GPIO pulse.
package Component.Time_At_Tone_Master.Implementation is

   -- The component class instance record:
   type Instance is new Time_At_Tone_Master.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Wait_Time_Ms : Natural - Number of milliseconds the master waits between the sending of the time message and the sending of the tone message. This is implemented internally by an Ada 'delay until' statement.
   -- Sync_Period : Positive - The number of ticks between sending clock sync messages.
   -- Enabled_State : Tat_State.Tat_State_Type - Is time at tone enabled or disabled by default at startup.
   --
   overriding procedure Init (Self : in out Instance; Wait_Time_Ms : in Natural; Sync_Period : in Positive := 1; Enabled_State : in Tat_State.Tat_State_Type := Tat_State.Enabled);

private

   -- Protected object for protecting data that can be modified by synchronous command.
   package Protected_Natural_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_32);
   package Protected_Boolean is new Protected_Variables.Generic_Variable (Boolean);

   -- The component class instance record:
   type Instance is new Time_At_Tone_Master.Base_Instance with record
      -- Static configuration:
      Wait_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (100);
      Sync_Period : Positive := Positive'First;
      -- Protected variables that can be set synchronously in commands:
      Send_Counter : Protected_Natural_Counter.Counter;
      Do_Sync_Once : Protected_Boolean.Variable;
      -- Number of time at tone transactions sent:
      Transaction_Count : Unsigned_32 := 0;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick used to trigger the sending of time messages.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Time_Message_Send message is dropped due to a full queue.
   overriding procedure Time_Message_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is null;
   -- This procedure is called when a Tone_Message_Send message is dropped due to a full queue.
   overriding procedure Tone_Message_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Time at Tone Master component.
   -- This enables the sending of time at tone messages.
   overriding function Enable_Time_At_Tone (Self : in out Instance) return Command_Execution_Status.E;
   -- This enables the sending of time at tone messages.
   overriding function Disable_Time_At_Tone (Self : in out Instance) return Command_Execution_Status.E;
   -- This sends a time at tone message followed by a tone message at the next tick, regardless of the current sync period. This is useful during testing to send a sync one time.
   overriding function Sync_Once (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Time_At_Tone_Master.Implementation;
