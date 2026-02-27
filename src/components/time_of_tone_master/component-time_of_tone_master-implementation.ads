--------------------------------------------------------------------------------
-- Time_Of_Tone_Master Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Sys_Time;
with Command;
with Protected_Variables;

-- This is the Time of Tone Master component. It provides an alternate implementation to the Time at Tone Master component also provided within Adamant. This implementation could potentially provide more accurate time syncing if your system has the ability to accurately determine the time at which the tone is sent outside of this component (i.e. the time when the tone leaves the serial port). If your tone is implemented in hardware (i.e. a GPIO signal) then the standard Time at Tone Master implementation will probably be more accurate.
--
-- TaT is a protocol used to sync a slave clock to a master clock. Two messages are sent from the master to the slave component. First a 'tone' message is sent which signals to the slave clock to save its current time. Next, a 'time' message is sent which provides the master time at which the 'tone' was sent. This time combined with the time the slave recorded when the 'tone' was received can be used to calculate a time delta of the slave clock with respect to the master. This component implements the master side of the protocol. This component outputs the time message and the tone as Tick.T send connectors. This design is intended to be generic enough to implement time at tone in many different manners on the other end of these connectors. For instance, you could convert the time message Tick.T to a CCSDS packet and the tone Tick.T to a GPIO pulse.
package Component.Time_Of_Tone_Master.Implementation is

   -- The component class instance record:
   type Instance is new Time_Of_Tone_Master.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Sync_Period : Positive - The number of ticks between sending clock sync messages.
   -- Enabled_State : Tat_State.Tat_State_Type - Is time at tone enabled or disabled by default at startup.
   --
   overriding procedure Init (Self : in out Instance; Sync_Period : in Positive := 1; Enabled_State : in Tat_State.Tat_State_Type := Tat_State.Enabled);

private

   -- Protected object for protecting data that can be modified by synchronous command.
   package Protected_Natural_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_32);
   package Protected_Boolean is new Protected_Variables.Generic_Variable (Boolean);

   -- The component class instance record:
   type Instance is new Time_Of_Tone_Master.Base_Instance with record
      -- Static configuration:
      Sync_Period : Positive := Positive'First;
      -- Protected variables that can be set synchronously in commands:
      Send_Counter : Protected_Natural_Counter.Counter;
      Do_Sync_Once : Protected_Boolean.Variable;
      -- Number of time at tone transactions sent:
      Tone_Message_Count : Unsigned_32 := 0;
      Time_Message_Count : Unsigned_32 := 0;
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
   -- Tick used to trigger the sending of tone messages.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- This connector triggers the sending of the time message. The time received here is assumed to be an accurate time stamp of when the tone message was sent. This time can be provided to this component by a lower level component which actually records the time the tone message leaves the system.
   overriding procedure Tone_Message_Sys_Time_T_Recv_Sync (Self : in out Instance; Arg : in Sys_Time.T);
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
   --    Commands for the Time of Tone Master component.
   -- This enables the sending of time at tone messages.
   overriding function Enable_Time_At_Tone (Self : in out Instance) return Command_Execution_Status.E;
   -- This enables the sending of time at tone messages.
   overriding function Disable_Time_At_Tone (Self : in out Instance) return Command_Execution_Status.E;
   -- This sends a time at tone message followed by a tone message at the next tick, regardless of the current sync period. This is useful during testing to send a sync one time.
   overriding function Sync_Once (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Time_Of_Tone_Master.Implementation;
