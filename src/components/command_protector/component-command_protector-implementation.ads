--------------------------------------------------------------------------------
-- Command_Protector Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Command;
with Packed_Arm_Timeout;
with Binary_Tree;
with Command_Types; use Command_Types;
with Arm_State;

-- This component is initialized with a list of protected commands. The component receives commands, and checks their IDs against the protected commands list. If a command is found in the list, then it is only forwarded if the component has been 'armed', otherwise the command is dropped and an error packet is produced with the rejected command data. Commands that are not on the protected commands list are always forwarded. To 'arm' the component, a special 'arm' command must be sent to the component to transition it to the 'armed' state. At this point, a protected command may be successfully forwarded. Note that after the receipt of any command, the component will transition back to the 'unarmed' state, rejecting any subsequently received protected commands until another 'arm' command is received. The component will also transition back to the 'unarmed' state after a timeout expires, which is set in the 'arm' command itself. The protected command list is stored internally as a binary tree data structure that can determine if a command is protected or not in O(log(n)) time, where n is the number of protected commands. Since most systems only manage a handful of protected commands, the performance of this component should be acceptable for most missions.
package Component.Command_Protector.Implementation is

   -- The component class instance record:
   type Instance is new Command_Protector.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of protected command IDs at initialization.
   --
   -- Init Parameters:
   -- Protected_Command_Id_List : Command_Id_List - The list of command IDs to consider as protected commands.
   --
   overriding procedure Init (Self : in out Instance; Protected_Command_Id_List : in Command_Id_List);
   not overriding procedure Final (Self : in out Instance);

private

   -- Define the protected command binary tree package:
   function Less_Than (Left, Right : in Command_Types.Command_Id) return Boolean is (Left < Right)
      with Inline => True;
   function Greater_Than (Left, Right : in Command_Types.Command_Id) return Boolean is (Left > Right)
      with Inline => True;
   package Protected_Command_B_Tree is new Binary_Tree (Command_Types.Command_Id, Less_Than, Greater_Than);

   -- The component class instance record:
   type Instance is new Command_Protector.Base_Instance with record
      -- Internal arm state and timeout:
      Command_Arm_State : Arm_State.Protected_Arm_State;
      -- Internal protected command list implemented as a binary tree:
      Protected_Command_List : Protected_Command_B_Tree.Instance;
      -- Some info counters for telemetry:
      Protected_Command_Reject_Count : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
      Protected_Command_Forward_Count : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
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
   -- Commands received on this connector will be checked against the protected command list and rejected if the system is 'unarmed'. Commands not found in the protected command list they will be forwarded.
   overriding procedure Command_T_To_Forward_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- The command receive connector for this component's specific commands.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is null;
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
   --    These are the commands for the Command Protector component.
   -- Transition the component to the 'arm' state. A timeout is provided, which when expires, will transition the component back to the 'unarmed' state, unless a command is received first.
   overriding function Arm (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Command_Protector.Implementation;
