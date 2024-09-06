-- This is a somewhat generic, unprotected binary tree for holding apids and filter factors for the downsampler component.
package body Apid_Tree is

   ---------------------------------------
   -- Binary tree comparison operators:
   ---------------------------------------
   function Less_Than (Left, Right : Ccsds_Downsampler_Tree_Entry) return Boolean is
   begin
      return Left.Apid < Right.Apid;
   end Less_Than;
   function Greater_Than (Left, Right : Ccsds_Downsampler_Tree_Entry) return Boolean is
   begin
      return Left.Apid > Right.Apid;
   end Greater_Than;

   procedure Init (Self : in out Instance; Downsample_List : in Ccsds_Downsample_Packet_List_Access) is
      Add_Status : Boolean;
      Search_Status : Boolean;
      Ignore_1 : Positive;
      Ignore_2 : Ccsds_Downsampler_Tree_Entry;
   begin
      -- Allocate space for the table:
      Self.Downsample_Entry.Init (Downsample_List'Length);
      -- For each item in the list, add the apid and filter factor to the internal tree
      for Id of Downsample_List.all loop
         -- Make sure we dont add multiple of the same apid
         Search_Status := Self.Downsample_Entry.Search (((Apid => Id.Apid, Filter_Factor => 1, Filter_Count => 0)), Ignore_2, Ignore_1);
         pragma Assert (not Search_Status, "Downsampler tree cannot add multiple nodes of the same APID.");

         Add_Status := Self.Downsample_Entry.Add (((Apid => Id.Apid, Filter_Factor => Id.Filter_Factor, Filter_Count => 0)));
         -- Make sure we dont get a failure for some reason
         pragma Assert (Add_Status, "Downsampler tree too small to hold all APIDs in the input list.");
      end loop;
   end Init;

   function Filter_Packet (Self : in out Instance; Apid : in Ccsds_Apid_Type; Count : out Unsigned_16) return Filter_Action_Status is
      Fetched_Entry : Ccsds_Downsampler_Tree_Entry;
      Tree_Index : Positive;
      Return_Status : Filter_Action_Status;
      Search_Status : constant Boolean := Self.Downsample_Entry.Search (((Apid => Apid, Filter_Factor => 1, Filter_Count => 0)), Fetched_Entry, Tree_Index);
   begin
      case Search_Status is
         -- If we couldn't find the packet, then increment the pass count and move on
         when False =>
            -- Update the counter and return the status
            Self.Num_Passed_Packets := @ + 1;
            Count := Self.Num_Passed_Packets;
            Return_Status := Invalid_Id;
         when True =>
            -- Check if we are filtering all first
            case Fetched_Entry.Filter_Factor is
               -- When the factor is set to 0, we dont pass anything along.
               when 0 =>
                  -- Update counter
                  Self.Num_Filtered_Packets := @ + 1;
                  Return_Status := Filter;
                  Count := Self.Num_Filtered_Packets;
               -- Use the filter factor value for all other values to determine if it needs to be filtered or not
               when others =>
                  if (Fetched_Entry.Filter_Count mod Fetched_Entry.Filter_Factor) = 0 then
                     Self.Num_Passed_Packets := @ + 1;
                     Return_Status := Pass;
                     Count := Self.Num_Passed_Packets;
                  else
                     -- Filtered here
                     Self.Num_Filtered_Packets := @ + 1;
                     Return_Status := Filter;
                     Count := Self.Num_Filtered_Packets;
                  end if;
            end case;
            -- If we found the entry in the tree, then make sure we update with the new count for that entry
            Fetched_Entry.Filter_Count := @ + 1;
            Self.Downsample_Entry.Set (Tree_Index, Fetched_Entry);
      end case;

      return Return_Status;
   end Filter_Packet;

   function Set_Filter_Factor (Self : in out Instance; Apid : in Ccsds_Apid_Type; New_Filter_Factor : in Unsigned_16; Tree_Index : out Positive) return Filter_Factor_Set_Status is
      Fetched_Entry : Ccsds_Downsampler_Tree_Entry;
      Index : Positive;
      Search_Status : constant Boolean := Self.Downsample_Entry.Search ((Apid => Apid, Filter_Factor => 1, Filter_Count => 0), Fetched_Entry, Index);
   begin
      -- set the index output variable just in case we dont find the entry
      Tree_Index := Positive'First;
      case Search_Status is
         when True =>
            -- set the new filter factor and reset the count
            Fetched_Entry.Filter_Factor := New_Filter_Factor;
            Fetched_Entry.Filter_Count := 0;
            -- Save the entry
            Self.Downsample_Entry.Set (Index, Fetched_Entry);
            Tree_Index := Index;
            return Success;
         when False =>
            -- Let the command know that we could not find the apid and seet the index to some value
            return Invalid_Id;
      end case;
   end Set_Filter_Factor;

   function Get_Tree_First_Index (Self : in Instance) return Positive is
   begin
      return Self.Downsample_Entry.Get_First_Index;
   end Get_Tree_First_Index;

   function Get_Tree_Last_Index (Self : in Instance) return Natural is
   begin
      return Self.Downsample_Entry.Get_Last_Index;
   end Get_Tree_Last_Index;

   function Get_Tree_Entry (Self : in Instance; Index : in Positive) return Ccsds_Downsampler_Tree_Entry is
   begin
      return Self.Downsample_Entry.Get (Index);
   end Get_Tree_Entry;

end Apid_Tree;
