--------------------------------------------------------------------------------
-- Ccsds_Product_Extractor Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Binary_Tree;
with Ccsds_Space_Packet;
with Product_Extractor_Types; use Product_Extractor_Types;

-- The product extractor is a component that extracts data from an incoming packet which it then creates into a data product and sends as its own data product for other component use. This is performed by using a list of types that include the offset and the corresponding APID to know which packets to extract from. The data is verified against the respective type at which point it will either send it on or create an event indicating there was an error. All of this information is derived from a user defined YAML input model that contains the information for each data product and the residing packet. See the generator documentation for more information.
package Component.Ccsds_Product_Extractor.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Product_Extractor.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Data_Product_Extraction_List : Product_Extractor_Types.Extracted_Product_List_Access - The list of data products that will be extracted from packets.
   --
   overriding procedure Init (Self : in out Instance; Data_Product_Extraction_List : in not null Product_Extractor_Types.Extracted_Product_List_Access);

   function Less_Than (Left, Right : Ccsds_Product_Apid_List) return Boolean with
      Inline => True;
   function Greater_Than (Left, Right : Ccsds_Product_Apid_List) return Boolean with
      Inline => True;
   package Ccsds_Product_Tree is new Binary_Tree (Ccsds_Product_Apid_List, Less_Than, Greater_Than);

private

   -- The component class instance record:
   type Instance is new Ccsds_Product_Extractor.Base_Instance with record
      Extracted_Products_Tree : Ccsds_Product_Tree.Instance;
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
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector that will receive the CCSDS space packets and extract data products if necessary
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

end Component.Ccsds_Product_Extractor.Implementation;
