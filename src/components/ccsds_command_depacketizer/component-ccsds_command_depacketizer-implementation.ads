--------------------------------------------------------------------------------
-- Ccsds_Command_Depacketizer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Ccsds_Space_Packet;
with Command;
with Interfaces;
with Protected_Variables;

-- This component receives CCSDS packets, validates the data within them, and converts them into Adamant commands. Note that the only internal state that this component contains is a packet accept and packet reject count. The component assumes that only a single task is attached to its CCSDS Space Packet invokee connector, and thus these counters are unprotected. If more than one task is attached to the input, a race condition arises around the counters, which may need to become protected.
package Component.Ccsds_Command_Depacketizer.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Command_Depacketizer.Base_Instance with private;

private

   -- Instantiate protected 8 bit counter:
   package Sixteen_Counter is new Protected_Variables.Generic_Protected_Counter (Interfaces.Unsigned_16);

   -- The component class instance record:
   type Instance is new Ccsds_Command_Depacketizer.Base_Instance with record
      Rejected_Packet_Count : Sixteen_Counter.Counter;
      Accepted_Packet_Count : Sixteen_Counter.Counter;
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
   -- The ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the component.
   -- This command resets the internal counts for the data products.
   overriding function Reset_Counts (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Ccsds_Command_Depacketizer.Implementation;
