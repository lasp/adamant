--------------------------------------------------------------------------------
-- Ccsds_Downsampler Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Apid_Tree;
with Ccsds_Downsampler_Types;
with Ccsds_Space_Packet;
with Ccsds_Primary_Header;
with Command;

-- The CCSDS downsampler is a component that is intended to filter down packets that are listed in the downsample list. The input list has two items, one is the APID, and the other is the filter factor. The filter factor is used to know the cadence of filtering and sending packets. This is maintained by a protected binary tree object which takes the APID of the packets from the input list, and adding them to a binary tree with the filter factor. When the packet is received, the APID is checked for filtering and then the filter factor to determine if we send them on or not. Packets that are not in the input list will not be filtered and sent as normal. As a note, the larger that the downsampled list is, the more there is to check in the supporting binary tree. It's recommended that the downsampled list contain less than a couple hundred items.
package Component.Ccsds_Downsampler.Implementation is
   use Ccsds_Primary_Header;
   use Ccsds_Downsampler_Types;

   -- The component class instance record:
   type Instance is new Ccsds_Downsampler.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Downsample_List : Ccsds_Downsampler_Types.Ccsds_Downsample_Packet_List_Access - The list of APIDs that are to be downsampled and the initial filter factor associated with those APIDs.
   --
   overriding procedure Init (Self : in out Instance; Downsample_List : in not null Ccsds_Downsampler_Types.Ccsds_Downsample_Packet_List_Access);

private
   -- Helper procedure that will take a tree entry and create a data product for the filter factor
   procedure Send_Filter_Data_Product (Self : in out Instance; Tree_Entry : in Ccsds_Downsampler_Tree_Entry; Tree_Index : in Positive);
   -- Protected type for a binary tree that maintains the information for each apid
   protected type Protected_Apid_Entries is
      -- init
      procedure Init (Downsample_List : in Ccsds_Downsample_Packet_List_Access);
      -- Procedure to fetch the event range. This helps keep the component in sync with the package
      procedure Filter_Packet (Apid : in Ccsds_Apid_Type; Count : out Unsigned_16; Status : out Apid_Tree.Filter_Action_Status);
      -- Procedure to get the pointer for the array. This is so that we can quickly copy the whole thing into the state packet
      procedure Set_Filter_Factor (Apid : in Ccsds_Apid_Type; New_Filter_Factor : in Unsigned_16; Tree_Index : out Positive; Status : out Apid_Tree.Filter_Factor_Set_Status);
      -- Functions to get the first and last index of the tree
      function Get_Tree_First_Index return Positive;
      function Get_Tree_Last_Index return Natural;
      -- Function to get the element when given an index.
      function Get_Tree_Entry (Index : in Positive) return Ccsds_Downsampler_Tree_Entry;

   private
      Apid_Tree_Package : Apid_Tree.Instance;
   end Protected_Apid_Entries;

   -- The component class instance record:
   type Instance is new Ccsds_Downsampler.Base_Instance with record
      -- Apid tree object
      Apid_Entries : Protected_Apid_Entries;
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
   -- This connector is the input connector for the packets that are coming in. This is where packets are checked for filtering.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;
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
   --    These are the commands for the ccsds downsampler component.
   -- Modify the filter factor of a specified APID. A value of 0 will filter all packets of that ID.
   overriding function Modify_Filter_Factor (Self : in out Instance; Arg : in Filter_Factor_Cmd_Type.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Ccsds_Downsampler.Implementation;
