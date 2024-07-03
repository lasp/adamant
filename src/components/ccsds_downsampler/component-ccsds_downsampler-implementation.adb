--------------------------------------------------------------------------------
-- Ccsds_Downsampler Component Implementation Body
--------------------------------------------------------------------------------

with Data_Product;
with Packed_U16;

package body Component.Ccsds_Downsampler.Implementation is

   ---------------------------------------
   -- Protected Event_Filter_Entries Wrapper:
   ---------------------------------------
   -- This package is used only with this component and is wrapped as a protected object to protect the manipulation of entry data for each ID on its filtered state.
   protected body Protected_Apid_Entries is
      procedure Init (Downsample_List : in Ccsds_Downsample_Packet_List_Access) is
      begin
         Apid_Tree_Package.Init (Downsample_List);
      end Init;
      -- Procedure to set the filter state by command.
      procedure Filter_Packet (Apid : in Ccsds_Apid_Type; Count : out Unsigned_16; Status : out Apid_Tree.Filter_Action_Status) is
         use Apid_Tree;
      begin
         Status := Apid_Tree_Package.Filter_Packet (Apid, Count);
      end Filter_Packet;
      -- Procedure to determine if the event needs to be filtered and if so, reports that to the component for further handling.
      procedure Set_Filter_Factor (Apid : in Ccsds_Apid_Type; New_Filter_Factor : in Unsigned_16; Tree_Index : out Positive; Status : out Apid_Tree.Filter_Factor_Set_Status) is
      begin
         Status := Apid_Tree_Package.Set_Filter_Factor (Apid, New_Filter_Factor, Tree_Index);
      end Set_Filter_Factor;

      function Get_Tree_First_Index return Positive is
      begin
         return Apid_Tree_Package.Get_Tree_First_Index;
      end Get_Tree_First_Index;

      function Get_Tree_Last_Index return Natural is
      begin
         return Apid_Tree_Package.Get_Tree_Last_Index;
      end Get_Tree_Last_Index;

      -- Function to get the element when given an index.
      function Get_Tree_Entry (Index : in Positive) return Ccsds_Downsampler_Tree_Entry is
      begin
         return Apid_Tree_Package.Get_Tree_Entry (Index);
      end Get_Tree_Entry;

   end Protected_Apid_Entries;

   -- Helper procedure to send a data product
   procedure Send_Filter_Data_Product (Self : in out Instance; Tree_Entry : in Ccsds_Downsampler_Tree_Entry; Tree_Index : in Positive) is
      use Data_Product_Types;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Dp_Id : constant Data_Product_Id := Self.Data_Products.Get_Id_Base + Data_Product_Id (Tree_Index - 1) + Data_Product_Id (Ccsds_Downsampler_Data_Products.Num_Data_Products);
      Dp : Data_Product.T := (Header => (Id => Dp_Id, Time => Timestamp, Buffer_Length => Packed_U16.Serialization.Serialized_Length), Buffer => [others => 0]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_U16.Serialization.Serialized_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => Tree_Entry.Filter_Factor));
      Self.Data_Product_T_Send_If_Connected ((Dp));
   end Send_Filter_Data_Product;

   --------------------------------------------------
   -- Subprogram for implementation Set_Up method:
   --------------------------------------------------
   overriding procedure Set_Up (Self : in out Instance) is
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      First_Index : constant Positive := Self.Apid_Entries.Get_Tree_First_Index;
      Last_Index : constant Natural := Self.Apid_Entries.Get_Tree_Last_Index;
   begin
      -- Set the component state and init the data products to 0 using our variables that will update the data products later
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Packets_Filtered (Timestamp, ((Value => 0))));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Packets_Passed (Timestamp, ((Value => 0))));
      -- Send out data products that are constructed from the init list. This means we have to find each one in the list
      for Idx in First_Index .. Last_Index loop
         Self.Send_Filter_Data_Product (Self.Apid_Entries.Get_Tree_Entry (Idx), Idx);
      end loop;
   end Set_Up;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Downsample_List : Ccsds_Downsampler_Types.Ccsds_Downsample_Packet_List_Access - The list of APIDs that are to be downsampled and the initial filter factor associated with those APIDs.
   --
   overriding procedure Init (Self : in out Instance; Downsample_List : in not null Ccsds_Downsampler_Types.Ccsds_Downsample_Packet_List_Access) is
   begin
      -- Make sure that we have an non-empty initial list
      pragma Assert (Downsample_List'Length /= 0, "Downsampler init list cannot be empty.");
      -- For each item in the list, add the apid and filter factor to the internal tree
      Self.Apid_Entries.Init (Downsample_List);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is the input connector for the packets that are coming in. This is where packets are checked for filtering.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      use Apid_Tree;
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
      Filter_Status : Filter_Action_Status;
      Count_Update : Unsigned_16;
   begin
      -- Start by searching for the current incoming APID and use that information to know if we need to pass the packet along or not.
      Self.Apid_Entries.Filter_Packet (Arg.Header.Apid, Count_Update, Filter_Status);

      case Filter_Status is
         -- Act based on what the tree told us to do
         when Pass =>
            Self.Ccsds_Space_Packet_T_Send_If_Connected (Arg);
            -- Update and send data products
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Packets_Passed (Timestamp, ((Value => Count_Update))));
         when Filter =>
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Packets_Filtered (Timestamp, ((Value => Count_Update))));
         when Invalid_Id =>
            Self.Ccsds_Space_Packet_T_Send_If_Connected (Arg);
            -- Update and send data products
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Total_Packets_Passed (Timestamp, ((Value => Count_Update))));
      end case;
   end Ccsds_Space_Packet_T_Recv_Sync;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the ccsds downsampler component.
   -- Modify the filter factor of a specified APID. A value of 0 will filter all packets of that ID.
   overriding function Modify_Filter_Factor (Self : in out Instance; Arg : in Filter_Factor_Cmd_Type.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Apid_Tree;
      Status : Filter_Factor_Set_Status;
      Index : Positive;
   begin
      -- Get the entry if it exist
      Self.Apid_Entries.Set_Filter_Factor (Arg.Apid, Arg.Filter_Factor, Index, Status);

      case Status is
         when Success =>
            Self.Event_T_Send_If_Connected (Self.Events.Modified_Factor_Filter (Self.Sys_Time_T_Get, Arg));
            -- If successful, then get the entry back and send the data product to also verify
            Self.Send_Filter_Data_Product (Self.Apid_Entries.Get_Tree_Entry (Index), Index);
            return Success;
         when Invalid_Id =>
            Self.Event_T_Send_If_Connected (Self.Events.Factor_Filter_Change_Failed_Invalid_Apid (Self.Sys_Time_T_Get, Arg));
            return Failure;
      end case;
   end Modify_Filter_Factor;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
         Self.Sys_Time_T_Get,
         (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
      null;
   end Invalid_Command;

end Component.Ccsds_Downsampler.Implementation;
