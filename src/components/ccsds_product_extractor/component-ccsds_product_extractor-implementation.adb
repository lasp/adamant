--------------------------------------------------------------------------------
-- Ccsds_Product_Extractor Component Implementation Body
--------------------------------------------------------------------------------

with Ccsds_Primary_Header; use Ccsds_Primary_Header;
with Invalid_Product_Data;

package body Component.Ccsds_Product_Extractor.Implementation is

   ---------------------------------------
   -- Binary tree comparison operators:
   ---------------------------------------
   function Less_Than (Left, Right : Ccsds_Product_Apid_List) return Boolean is
   begin
      return Left.Apid < Right.Apid;
   end Less_Than;

   function Greater_Than (Left, Right : Ccsds_Product_Apid_List) return Boolean is
   begin
      return Left.Apid > Right.Apid;
   end Greater_Than;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Data_Product_Extraction_List : Product_Extractor_Types.Extracted_Product_List_Access - The list of data products that will be extracted from packets.
   --
   overriding procedure Init (Self : in out Instance; Data_Product_Extraction_List : in not null Product_Extractor_Types.Extracted_Product_List_Access) is
      Add_Status : Boolean;
      Search_Status : Boolean;
      Fetched_Entry : Ccsds_Product_Apid_List;
      Ignore : Positive;
   begin
      -- Allocate space for the table:
      Self.Extracted_Products_Tree.Init (Data_Product_Extraction_List'Length);

      -- Loop through the list of all the products and save them to the binary tree
      for Packet_Products of Data_Product_Extraction_List.all loop
         -- Fill in the binary tree with the products from the list. Assert that we are not adding a duplicate APID in.
         Search_Status := Self.Extracted_Products_Tree.Search (Packet_Products, Fetched_Entry, Ignore);
         pragma Assert (not Search_Status, "Apid in the tree already exists!");
         Add_Status := Self.Extracted_Products_Tree.Add (Packet_Products);
         pragma Assert (Add_Status, "Product Extractor tree can not hold all APIDs and associated data in the input list.");
      end loop;
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector that will receive the CCSDS space packets and extract data products if necessary
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Fetched_Entry : Ccsds_Product_Apid_List;
      Ignore : Positive;
      -- Search the tree for the apid of the packet that was just received. If found, there are products to extract
      Search_Status : constant Boolean := Self.Extracted_Products_Tree.Search (((Apid => Arg.Header.Apid, Extract_List => null)), Fetched_Entry, Ignore);
   begin
      case Search_Status is
         -- Nothing to extract if the packet APID was not listed
         when False =>
            null;
         -- When an id is found, loop through all the extracted data products, create the data product, and verify they are in a valid range before sending them on
         when True =>
            for Idx in Fetched_Entry.Extract_List.all'Range loop
               -- Determine which timestamp to use based on the yaml input
               declare
                  Extracted_Product : Data_Product.T;
                  Invalid_Data_Product : Invalid_Product_Data.T;
                  Product_Entry_List_Item : constant Extract_And_Validate := Fetched_Entry.Extract_List.all (Idx);
                  Product_Extracted_Status : constant Product_Status := Product_Entry_List_Item.all (Arg, Self.Data_Products.Get_Id_Base, Timestamp, Extracted_Product, Invalid_Data_Product);
               begin
                  case Product_Extracted_Status is
                     -- If there was a problem with the length of the data product and the length of the incoming packet, then send an event.
                     when Length_Error =>
                        Self.Event_T_Send_If_Connected (Self.Events.Invalid_Extracted_Product_Length (Timestamp, (Id => Extracted_Product.Header.Id, Apid => Arg.Header.Apid, Length => Arg.Header.Packet_Length)));
                     -- If there was a problem with the value of the data relative to what was defined, then send an event which is done by the Invalid_Data_Event handler
                     when Invalid_Data =>
                        Self.Event_T_Send_If_Connected (Self.Events.Invalid_Extracted_Product_Data (Timestamp, Invalid_Data_Product));
                     -- Otherwise, send the extracted data product from the packet.
                     when Success =>
                        Self.Data_Product_T_Send_If_Connected ((Extracted_Product));
                  end case;
               end;
            end loop;
      end case;
   end Ccsds_Space_Packet_T_Recv_Sync;

end Component.Ccsds_Product_Extractor.Implementation;
