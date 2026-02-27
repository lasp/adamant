--------------------------------------------------------------------------------
-- Command_Rejector Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Binary_Tree;
with Command_Types; use Command_Types;
with Interfaces;

-- This component is initialized with a list of commands to reject. The component receives commands, and checks their IDs against the reject command list. If a command is found in the list, then it is dropped and reported as an error packet. Commands that are not on the reject list are always forwarded. The reject command list is stored internally as a binary tree data structure that can determine if a command should be rejected or not in O(log(n)) time, where n is the number of commands to reject. Since most systems only manage a handful of commands on the reject list, the performance of this component should be acceptable for most missions. A common application for this component is to actively disallow commands emanating from certain sources, such as an onboard command sequence.
package Component.Command_Rejector.Implementation is

   -- The component class instance record:
   type Instance is new Command_Rejector.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of command ID to reject at initialization.
   --
   -- Init Parameters:
   -- Command_Id_Reject_List : Command_Id_List - The list of command IDs to reject.
   --
   overriding procedure Init (Self : in out Instance; Command_Id_Reject_List : in Command_Id_List);
   not overriding procedure Final (Self : in out Instance);

private

   -- Define the command reject binary tree package:
   function Less_Than (Left, Right : in Command_Types.Command_Id) return Boolean is (Left < Right)
      with Inline => True;
   function Greater_Than (Left, Right : in Command_Types.Command_Id) return Boolean is (Left > Right)
      with Inline => True;
   package Command_Reject_B_Tree is new Binary_Tree (Command_Types.Command_Id, Less_Than, Greater_Than);

   -- The component class instance record:
   type Instance is new Command_Rejector.Base_Instance with record
      -- Internal command reject list implemented as a binary tree:
      Command_Reject_List : Command_Reject_B_Tree.Instance;
      -- Some info counters for telemetry:
      Command_Reject_Counter : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
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
   -- Commands received on this connector will be checked against the command reject list. Commands not found in the command reject list they will be forwarded.
   overriding procedure Command_T_To_Forward_Recv_Sync (Self : in out Instance; Arg : in Command.T);

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

end Component.Command_Rejector.Implementation;
