--------------------------------------------------------------------------------
-- Register_Stuffer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Arm_State;

-- This component services commands to stuff and dump registers. This component is different than the memory stuffer/dumper in that it atomically sets 32-bit little endian registers, which is a requirement on some hardware. It rejects commands to stuff or dump addresses that are not on a 4-byte boundary. Note that this component assumes all registers it accesses are little endian. Another version of this component needs to be created to access registers as big endian.
package Component.Register_Stuffer.Implementation is

   -- The component class instance record:
   type Instance is new Register_Stuffer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Configuration for the register stuffer component.
   --
   -- Init Parameters:
   -- Protect_Registers : Boolean - If set to True, the arm command will be required before each register write command. This does not affect register reads. If set to False, an arm command is not required before each register write command.
   --
   overriding procedure Init (Self : in out Instance; Protect_Registers : in Boolean);

private

   -- The component class instance record:
   type Instance is new Register_Stuffer.Base_Instance with record
      -- Internal arm state and timeout:
      Command_Arm_State : Arm_State.Protected_Arm_State;
      Protect_Registers : Boolean := False;
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
   -- This tick is used to keep track of the armed state timeout and send the data product relating the current timeout value.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Register Stuffer component.
   -- Write the value of a register.
   overriding function Write_Register (Self : in out Instance; Arg : in Register_Value.T) return Command_Execution_Status.E;
   -- Read the value of a register and reflect it in a data product.
   overriding function Read_Register (Self : in out Instance; Arg : in Packed_Address.T) return Command_Execution_Status.E;
   -- An arm command which enables the next write command to a register to be accepted. The armed state of the component will expire on the next command to this component no matter what it is or after the configurable timeout.
   overriding function Arm_Protected_Write (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) return Command_Execution_Status.E;
   -- Read the value of multiple registers and dump them into a packet.
   overriding function Dump_Registers (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Register_Stuffer.Implementation;
