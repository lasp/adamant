--------------------------------------------------------------------------------
-- Ccsds_Router Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Ccsds_Space_Packet;
with Ccsds_Primary_Header;
with Binary_Tree;

-- This component routes CCSDS packets to output connectors based on a static table matching APID to the output connector index. Table lookup is done by binary searching against APID. The look up returns a list of indexes which to route that packet. This component can receive packets either synchronously, or asynchronously and can be made either active or passive depending on the desired use case.
package Component.Ccsds_Router.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Router.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a routing table which maps CCSDS packet APIDs to a list of output connector indexes. This is provided as part of the initialization function.
   --
   -- Init Parameters:
   -- table : Ccsds_Router_Types.Router_Table_Entry_Array - An array of router table entries which include routing and sequence count checking information.
   -- Report_Unrecognized_Apids : Boolean - Should the component report unrecognized APIDs by sending out an error packet and event, True, or should it not report them at all, False.
   --
   overriding procedure Init (Self : in out Instance; Table : in Ccsds_Router_Types.Router_Table_Entry_Array; Report_Unrecognized_Apids : in Boolean := True);
   not overriding procedure Final (Self : in out Instance);

private
   use Ccsds_Router_Types;

   -- Define internal router table entry:
   type Internal_Router_Table_Entry is record
      -- Store an external table entry:
      Table_Entry : Router_Table_Entry;
      -- Extra data we need to make this component work internally:
      Last_Sequence_Count : Ccsds_Primary_Header.Ccsds_Sequence_Count_Type := Ccsds_Primary_Header.Ccsds_Sequence_Count_Type'Last;
   end record;

   -- Define the router table binary tree package:
   function Less_Than (Left, Right : Internal_Router_Table_Entry) return Boolean with
      Inline => True;
   function Greater_Than (Left, Right : Internal_Router_Table_Entry) return Boolean with
      Inline => True;
   package Router_Table_B_Tree is new Binary_Tree (Internal_Router_Table_Entry, Less_Than, Greater_Than);

   -- The component class instance record:
   type Instance is new Ccsds_Router.Base_Instance with record
      Table : Router_Table_B_Tree.Instance;
      Report_Unrecognized_Apids : Boolean := True;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The synchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- The asynchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Index : in Ccsds_Space_Packet_T_Send_Index; Arg : in Ccsds_Space_Packet.T) is null;
   -- This procedure is called when a Unrecognized_Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Unrecognized_Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

end Component.Ccsds_Router.Implementation;
