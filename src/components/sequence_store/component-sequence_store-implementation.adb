--------------------------------------------------------------------------------
-- Sequence_Store Component Implementation Body
--------------------------------------------------------------------------------

with System.Storage_Elements;
with Sequence_Store_Slot_Header;
with Sequence_Store_Enums;
with Memory_Region;
with Sequence_Header;
with Crc_16;
with Packed_Slot_Summary;
with Serializer_Types;
with Sequence_Util;
with Sequence_Store_Slot_Metadata;

package body Component.Sequence_Store.Implementation is

   use Sequence_Store_Types;

   -- Define the overhead byte associated with a slot in memory.
   Num_Slot_Meta_Bytes : constant Natural := Sequence_Store_Slot_Header.Size_In_Bytes - Sequence_Header.Size_In_Bytes;
   -- Check our constant to make sure it makes sense.
   pragma Compile_Time_Error (Num_Slot_Meta_Bytes /= Sequence_Store_Slot_Metadata.Size_In_Bytes, "Expected meta data bytes to equal size of metadata type.");

   ---------------------------------------
   -- Protected object implementation:
   ---------------------------------------

   protected body Protected_Sequence_Lookup_B_Tree is

      procedure Init (Maximum_Size : in Positive) is
      begin
         Tree.Init (Maximum_Size);
      end Init;

      procedure Destroy is
      begin
         Tree.Destroy;
      end Destroy;

      -- Add a sequence to the tree if a sequence of that ID doesn't already exist in the tree.
      procedure Add_Sequence (Id : in Sequence_Types.Sequence_Id; Slot : in Sequence_Store_Types.Slot_Number; Status : out Add_Status) is
         Element_To_Add : constant Sequence_Lookup_Element := (Id => Id, Slot => Slot);
         Ignore_Element : Sequence_Lookup_Element;
         Ignore_Index : Positive;
         Ret : Boolean;
      begin
         -- Initialize out parameter:
         Status := Success;

         -- First make sure that the sequence ID is not already found in the tree:
         if Tree.Search (Element => Element_To_Add, Element_Found => Ignore_Element, Element_Index => Ignore_Index) then
            Status := Duplicate_Id;
         else
            -- OK, there is no sequence with this ID found in the tree. Let's add it.
            Ret := Tree.Add (Element => Element_To_Add);
            pragma Assert (Ret, "If the tree is ever full during insertion then there is a bug in this implementation.");
            -- The tree contains one entry per slot. Thus, we should never have a situation where we need to store more entries
            -- then slots that exist unless there is a bug where entries are not appropriately being removed.
         end if;
      end Add_Sequence;

      -- Remove a sequence from the tree if it exists in the tree.
      procedure Remove_Sequence (Id : in Sequence_Types.Sequence_Id; Status : out Find_Status) is
         Element_To_Remove : constant Sequence_Lookup_Element := (Id => Id, Slot => Sequence_Store_Types.Slot_Number'First);
         Ignore_Element : Sequence_Lookup_Element;
         Index_To_Remove : Positive;
         Ret : Boolean;
      begin
         -- Initialize out parameter:
         Status := Success;

         -- First make sure that the sequence ID can found in the tree:
         if Tree.Search (Element => Element_To_Remove, Element_Found => Ignore_Element, Element_Index => Index_To_Remove) then
            -- OK, we found the element, remove it.
            Ret := Tree.Remove (Element_Index => Index_To_Remove);
            pragma Assert (Ret, "We just found this index. We should always be able to remove it.");
         else
            Status := Id_Not_Found;
         end if;
      end Remove_Sequence;

      -- Find a sequence in the tree if it exists.
      function Find_Sequence (Id : in Sequence_Types.Sequence_Id; Slot : out Sequence_Store_Types.Slot_Number) return Find_Status is
         Element_To_Find : constant Sequence_Lookup_Element := (Id => Id, Slot => Sequence_Store_Types.Slot_Number'First);
         Element_Found : Sequence_Lookup_Element;
         Ignore_Index : Positive;
      begin
         -- Initialize out parameter:
         Slot := Sequence_Store_Types.Slot_Number'First;

         -- First make sure that the sequence ID can found in the tree:
         if Tree.Search (Element => Element_To_Find, Element_Found => Element_Found, Element_Index => Ignore_Index) then
            -- Set the slot:
            Slot := Element_Found.Slot;
            return Success;
         else
            return Id_Not_Found;
         end if;
      end Find_Sequence;

   end Protected_Sequence_Lookup_B_Tree;

   --------------------------------------------------
   -- Helper subprograms
   --------------------------------------------------

   -- Given a slot number, make sure it is in range. This function throws an event
   -- if the number is out of range.
   function Is_Slot_Number_In_Range (Self : in out Instance; Slot_Index : in Slot_Number) return Boolean is
   begin
      if Slot_Index < Self.Slots.all'First or else Slot_Index > Self.Slots.all'Last then
         -- Throw event:
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Slot_Number (Self.Sys_Time_T_Get, (Slot => Slot_Index)));
         return False;
      else
         return True;
      end if;
   end Is_Slot_Number_In_Range;

   -- Given a slot index, return the header for that slot. Note that this
   -- function does not check the bounds of the Slot_Index, it assumes that the caller
   -- has already checked the bounds.
   function Get_Slot_Header (Self : in Instance; Slot_Index : in Slot_Number) return Sequence_Store_Slot_Header.T is
      -- Overlay sequence header over the beginning of the appropriate slot. We know that this overlay
      -- will not go over the end of the memory region, since we have checked the memory region's length
      -- in the Init, and ensured they are all at least as large as this header.
      To_Return : constant Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Self.Slots.all (Slot_Index).Address;
   begin
      return To_Return;
   end Get_Slot_Header;

   -- Given a slot index, set the header for that slot. Note that this
   -- function does not check the bounds of the Slot_Index, it assumes that the caller
   -- has already checked the bounds.
   procedure Set_Slot_Header (Self : in Instance; Slot_Index : in Slot_Number; Header : in Sequence_Store_Slot_Header.T) is
      -- Overlay sequence header over the beginning of the appropriate slot. We know that this overlay
      -- will not go over the end of the memory region, since we have checked the memory region's length
      -- in the Init, and ensured they are all at least as large as this header.
      To_Set : Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Self.Slots.all (Slot_Index).Address;
   begin
      To_Set := Header;
   end Set_Slot_Header;

   -- Given a slot number return the memory region that stores the sequence header and sequence data. This memory
   -- region excludes the slot meta data which is found at the beginning of each slot.
   function Get_Sequence_Memory_Region (Self : in Instance; Slot_Index : in Slot_Number) return Memory_Region.T is
      -- Overlay sequence header over the beginning of the appropriate slot. We know that this overlay
      -- will not go over the end of the memory region, since we have checked the memory region's length
      -- in the Init, and ensured they are all at least as large as this header.
      Slot_Header : constant Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Self.Slots.all (Slot_Index).Address;
   begin
      -- Return the address of the beginning of the sequence header and compute the length by using the memory
      -- region length which includes the size of the sequence header itself.
      return (Address => Slot_Header.Seq_Header'Address, Length => Slot_Header.Seq_Header.Length);
   end Get_Sequence_Memory_Region;

   -- Given a slot number return the maximum sized memory region that could store a   sequence header and sequence data at this slot.
   -- This memory region excludes the slot meta data which is found at the beginning of each slot.
   function Get_Max_Size_Sequence_Memory_Region (Self : in Instance; Slot_Index : in Slot_Number) return Memory_Region.T is
      -- Overlay sequence header over the beginning of the appropriate slot. We know that this overlay
      -- will not go over the end of the memory region, since we have checked the memory region's length
      -- in the Init, and ensured they are all at least as large as this header.
      Slot_Header : constant Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Self.Slots.all (Slot_Index).Address;
   begin
      -- Return the address of the beginning of the sequence header and compute the length by using the memory
      -- region length minus the size of the sequence slot header (not including the sequence header).
      return (Address => Slot_Header.Seq_Header'Address, Length => Self.Slots.all (Slot_Index).Length - Num_Slot_Meta_Bytes);
   end Get_Max_Size_Sequence_Memory_Region;

   -- CRC a sequence store slot.
   function Crc_Sequence_Store_Slot (Self : in Instance; Slot_Index : in Slot_Number) return Sequence_Store_Enums.Slot_Valid_Type.E is
      use Sequence_Util;
      use Sequence_Store_Enums.Slot_Valid_Type;
      -- Get the sequence memory region for this slot:
      Region : constant Memory_Region.T := Self.Get_Sequence_Memory_Region (Slot_Index);
      Ignore_1 : Sequence_Header.T;
      Ignore_2 : Crc_16.Crc_16_Type;
      -- Perform the CRC:
      Ret : constant Sequence_Util.Crc_Status := Crc_Sequence_Memory_Region (Region, Ignore_1, Ignore_2);
   begin
      case Ret is
         when Valid =>
            return Valid;
         when Crc_Error =>
            return Invalid;
         when Length_Error =>
            return Invalid;
      end case;
   end Crc_Sequence_Store_Slot;

   -- Check a sequence store slot, and mark the result of the CRC check in the slot header.
   function Check_Sequence_Store_Slot (Self : in Instance; Slot_Index : in Slot_Number) return Sequence_Store_Enums.Slot_Valid_Type.E is
      -- Get the header for this slot:
      Slot_Header : Sequence_Store_Slot_Header.T := Self.Get_Slot_Header (Slot_Index);
      Valid_Status : constant Sequence_Store_Enums.Slot_Valid_Type.E := Self.Crc_Sequence_Store_Slot (Slot_Index);
   begin
      -- Change the slot header validity state:
      Slot_Header.Slot_Info.Validity := Valid_Status;

      -- Save off the modified header:
      Self.Set_Slot_Header (Slot_Index, Slot_Header);
      return Valid_Status;
   end Check_Sequence_Store_Slot;

   -- Check all sequence store slots contained within the component, and modify their headers to reflect
   -- the results.
   procedure Check_All_Sequence_Store_Slots (Self : in Instance) is
      -- We don't care about the status of the checks. These will be reflected in the headers
      -- which can be transmitted in the summary packet.
      Ignore_Status : Sequence_Store_Enums.Slot_Valid_Type.E;
   begin
      for Slot_Index in Self.Slots.all'Range loop
         Ignore_Status := Self.Check_Sequence_Store_Slot (Slot_Index);
      end loop;
   end Check_All_Sequence_Store_Slots;

   procedure Send_Slot_Summary_Packet (Self : in out Instance) is
   begin
      if Self.Is_Packet_T_Send_Connected then
         declare
            -- Create byte array to hold summaries. We know that this byte array will fit into a packet, since we checked at
            -- initialization.
            subtype Summary_Byte_Array is Basic_Types.Byte_Array (0 .. Packed_Slot_Summary.Size_In_Bytes * Self.Slots.all'Length - 1);
            Summary_Bytes : Summary_Byte_Array := [others => 0];
            Idx : Natural := Summary_Bytes'First;
         begin
            -- For each slot, grab the header and put relevant data into the
            -- packet.
            for Slot_Index in Self.Slots.all'Range loop
               declare
                  -- Get the header for this slot:
                  Slot_Header : constant Sequence_Store_Slot_Header.T := Self.Get_Slot_Header (Slot_Index);
               begin
                  -- Store relevant header information into the packet bytes:
                  Summary_Bytes (Idx .. Idx + Packed_Slot_Summary.Size_In_Bytes - 1) := Packed_Slot_Summary.Serialization.To_Byte_Array ((
                     Slot_Info => Slot_Header.Slot_Info,
                     Id => Slot_Header.Seq_Header.Id,
                     Sequence_Length => Slot_Header.Seq_Header.Length
                  ));
                  -- Increment the index:
                  Idx := @ + Packed_Slot_Summary.Size_In_Bytes;
               end;
            end loop;

            -- OK we have all the packet data stored, now create and send out the packet:
            declare
               use Serializer_Types;
               -- Create the packet:
               Pkt : Packet.T;
               Stat : constant Serialization_Status := Self.Packets.Slot_Summaries (Timestamp => Self.Sys_Time_T_Get, Buf => Summary_Bytes, Pkt => Pkt);
            begin
               -- The creation of the packet can not fail, since we already verified that the
               -- summary bytes can indeed fit within the packet.
               pragma Assert (Stat = Success, "Autocode bug.");
               -- Send the packet:
               Self.Packet_T_Send (Pkt);
            end;
         end;
      end if;
   end Send_Slot_Summary_Packet;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory regions (slots) it is to manage.
   --
   -- Init Parameters:
   -- Sequence_Slots : Sequence_Slot_Array_Access - A list of memory regions. Each represents a slot to hold a single sequence of equal or smaller size (including header). This list will be copied into the component at initialization onto a heap allocated memory. The memory regions must not overlap in any way, must be large enough to at least hold a sequence header, and the list must not be empty. These properties will be enforced by the component via assertions when Init is called.
   -- Check_Slots_At_Startup : Boolean - If True, then check the validity of the sequences in all slots by computing CRCs over them at startup.
   -- Dump_Slot_Summary_At_Startup : Boolean - If True, then the slot summaries will be dumped at startup.
   --
   overriding procedure Init (Self : in out Instance; Sequence_Slots : in not null Sequence_Slot_Array_Access; Check_Slots_At_Startup : in Boolean; Dump_Slot_Summary_At_Startup : in Boolean) is
   begin
      -- Make sure the sequence slots exist:
      pragma Assert (Sequence_Slots.all'First = 0, "Slots must be zero indexed. This is so there is no conflict with the ground.");
      pragma Assert (Sequence_Slots.all'Length >= 1, "There must be at least one slot.");
      pragma Assert (Sequence_Slots.all'Last >= 0, "There must be at least one slot.");

      -- OK, now let's save off the pointer.
      Self.Slots := Sequence_Slots;

      -- First make sure all the slots can can fit a minimum size sequence:
      for Slot of Self.Slots.all loop
         pragma Assert (Slot.Length >= Sequence_Store_Slot_Header.Size_In_Bytes, "Sequence slot too small to hold minimum sized sequence.");
      end loop;

      -- We need to make sure none of the sequence slot memory regions overlap. If they do, this is
      -- a user error and should be caught here.
      for Idx in Self.Slots.all'Range loop
         for Jdx in Self.Slots.all'Range loop
            -- Don't compare the same memory region against itself.
            if Idx /= Jdx then
               declare
                  use System.Storage_Elements;
                  A : Memory_Region.T renames Self.Slots.all (Idx);
                  B : Memory_Region.T renames Self.Slots.all (Jdx);
                  A_Start : System.Address renames A.Address;
                  A_Stop : constant System.Address := A.Address + Storage_Offset (A.Length - 1);
                  B_Start : System.Address renames B.Address;
                  B_Stop : constant System.Address := B.Address + Storage_Offset (B.Length - 1);
                  -- Calculate offset between start addresses. Note: that there is no
                  -- > or < operators supplied for System.Address, so we use subtraction
                  -- instead to convert the offset to a Storage_Offset which supports these
                  -- comparison operators.
                  Offset : constant Storage_Offset := A_Start - B_Start;
               begin
                  -- Check assumptions, make sure regions are not zero length:
                  pragma Assert ((A_Stop - A_Start) > 0, "Bug in init.");
                  pragma Assert ((B_Stop - B_Start) > 0, "Bug in init.");

                  -- Check for overlap:
                  if Offset > 0 then
                     -- "a" region must be after "b" region.
                     pragma Assert ((A_Start - B_Stop) > 0, "Regions overlap.");
                  else
                     -- "a" region must be before "b" region.
                     pragma Assert ((B_Start - A_Stop) > 0, "Regions overlap.");
                  end if;
               end;
            end if;
         end loop;
      end loop;

      -- Allocate space for the active sequence list. The largest this can ever be is one
      -- entry per slot.
      Self.Active_Sequence_List.Init (Maximum_Size => Self.Slots.all'Length);

      -- Create the active sequence list. We need to look at all the sequence memory regions
      -- that were passed in. Any sequence that is active needs to be added to the list.
      for Slot_Index in Self.Slots.all'Range loop
         declare
            use Sequence_Store_Enums.Slot_State_Type;
            Slot_Header : Sequence_Store_Slot_Header.T := Self.Get_Slot_Header (Slot_Index);
            Status : Add_Status;
         begin
            case Slot_Header.Slot_Info.State is
               when Active =>
                  -- Add the sequence to the list:
                  Self.Active_Sequence_List.Add_Sequence (Id => Slot_Header.Seq_Header.Id, Slot => Slot_Index, Status => Status);

                  -- Check the return status:
                  case Status is
                     when Success =>
                        null; -- Nothing to do, all is good.
                     when Duplicate_Id =>
                        -- We have found two active sequences with the same ID. This is not good, and the sequence store
                        -- cannot operate under this condition. This is an operations error, or MRAM got corrupted. To fix
                        -- this we need to disable any duplicate sequence IDs that we find. We will only use the first active
                        -- duplicate sequence ID and hope that it is the best one. A CRC check can be done by downstream
                        -- components to ensure the sequence is valid. The ground can look at the sequence store summary
                        -- and fix any issues.

                        -- Mark the slot as inactive.
                        Slot_Header.Slot_Info.State := Inactive;
                        Self.Set_Slot_Header (Slot_Index, Slot_Header);
                  end case;
               when Inactive =>
                  null; -- Nothing to do.
            end case;
         end;
      end loop;

      -- Make sure that the slot summaries for all the slots in this component will fit in a
      -- single packet. This is an assumption that this component makes when creating the
      -- packet.
      pragma Assert (Packed_Slot_Summary.Size_In_Bytes * Self.Slots.all'Length <= Packet_Types.Packet_Buffer_Type'Length,
         "The slot summary is too large to fit in a single packet.");

      -- If configured to do so, we should check the validity of all the sequences at startup by
      -- computing CRCs over them and then updating their header information accordingly.
      if Check_Slots_At_Startup then
         Self.Check_All_Sequence_Store_Slots;
      end if;

      -- Save off dump boolean. We won't dump until Set_Up.
      Self.Dump_Slot_Summary_At_Startup := Dump_Slot_Summary_At_Startup;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Active_Sequence_List.Destroy;
   end Final;

   overriding procedure Set_Up (Self : in out Instance) is
   begin
      if Self.Dump_Slot_Summary_At_Startup then
         -- Send the slot summary at startup:
         Self.Send_Slot_Summary_Packet;
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- This connector is used to load a new sequence into a slot in the store.
   overriding procedure Sequence_Store_Memory_Region_Store_T_Recv_Async (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T) is
      use Sequence_Store_Enums.Sequence_Store_Status;
      Status_To_Return : Sequence_Store_Enums.Sequence_Store_Status.E := Success;
   begin
      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Writing_Sequence_To_Slot (Self.Sys_Time_T_Get, Arg));

      -- First let's make sure the slot number is in range:
      if not Self.Is_Slot_Number_In_Range (Arg.Slot) then
         -- Event already thrown in subprogram call above.
         Status_To_Return := Invalid_Slot_Number;
      else
         -- OK the slot checks out.
         -- We can only store this new sequence if the destination slot is not currently active. Let's
         -- check that.
         declare
            use Sequence_Store_Enums.Slot_State_Type;
            Slot_Header : Sequence_Store_Slot_Header.T := Self.Get_Slot_Header (Arg.Slot);
         begin
            case Slot_Header.Slot_Info.State is
               when Active =>
                  -- Throw event:
                  Self.Event_T_Send_If_Connected (Self.Events.Sequence_Slot_Active (Self.Sys_Time_T_Get, Arg));
                  Status_To_Return := Slot_In_Use;
               when Inactive =>
                  -- OK the slot is currently inactive, so we can store this new sequence.
                  -- Now let's check the sequence CRC and make sure it is valid.
                  declare
                     use Sequence_Util;
                     -- Calculate the CRC over this sequence region:
                     Header : Sequence_Header.T;
                     Computed_Crc : Crc_16.Crc_16_Type;
                     Ret : constant Sequence_Util.Crc_Status := Sequence_Util.Crc_Sequence_Memory_Region (Arg.Sequence_Region, Seq_Header => Header, Computed_Crc => Computed_Crc);
                  begin
                     if Ret /= Valid then
                        -- Throw event:
                        Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Crc (Self.Sys_Time_T_Get, (Store => Arg, Header => Header, Computed_Crc => Computed_Crc)));
                        Status_To_Return := Crc_Error;
                     else
                        -- OK the CRC checks out. Let's make sure the length of the sequence is not too large
                        -- for the store.
                        declare
                           -- Calculate the amount of bytes the sequence will take up in the slot, including the header
                           -- and slot meta data. Note that Header.Length includes the size of the sequence header, so
                           -- we need to subtract that size since its included in the Sequence_Store_Slot_Header.
                           Sequence_Size_In_Slot : constant Natural := Header.Length + Num_Slot_Meta_Bytes;
                           -- Grab the slot to store this sequence in.
                           Slot_Region : constant Memory_Region.T := Self.Slots.all (Arg.Slot);
                        begin
                           -- First let's check the length of the sequence and make sure the memory region is at least as large and that
                           -- the sequence size is large enough to include the slot header.
                           if Sequence_Size_In_Slot > Slot_Region.Length or else
                               Sequence_Size_In_Slot < Sequence_Store_Slot_Header.Size_In_Bytes
                           then
                              -- Throw event:
                              Self.Event_T_Send_If_Connected (Self.Events.Invalid_Sequence_Length (Self.Sys_Time_T_Get, (Store => Arg, Header => Header, Length_Bound => Slot_Region.Length)));
                              Status_To_Return := Length_Error;
                           else
                              -- OK everything is good. The destination slot exists and is inactive. The sequence to store
                              -- has a length that is not too large and a valid CRC. Let's store the sequence.
                              declare
                                 use Sequence_Store_Enums.Slot_Valid_Type;
                                 -- Calculate the size of the sequence including its header.
                                 Sequence_Size : constant Natural := Header.Length;
                                 -- Overlay the sequence with a byte array.
                                 subtype Safe_Byte_Array is Basic_Types.Byte_Array (0 .. Sequence_Size - 1);
                                 Sequence_Bytes : Safe_Byte_Array with Import, Convention => Ada, Address => Arg.Sequence_Region.Address;
                                    -- Get the sequence memory region for this slot:
                                 Slot_Sequence_Region : constant Memory_Region.T := Self.Get_Max_Size_Sequence_Memory_Region (Arg.Slot);
                                 -- Overlay the destination slot with a byte array.
                                 subtype Slot_Byte_Array is Basic_Types.Byte_Array (0 .. Slot_Sequence_Region.Length - 1);
                                 Slot_Bytes : Slot_Byte_Array with Import, Convention => Ada, Address => Slot_Sequence_Region.Address;
                                 Ignore_Stat : Command_Execution_Status.E;
                              begin
                                 -- Modify the sequence validity and write it back in the header.
                                 Slot_Header.Slot_Info.Validity := Valid;
                                 Self.Set_Slot_Header (Arg.Slot, Slot_Header);

                                 -- OK do the actual copy:
                                 Slot_Bytes (0 .. Sequence_Size - 1) := Sequence_Bytes;

                                 -- Throw info event:
                                 Self.Event_T_Send_If_Connected (Self.Events.Wrote_Sequence_To_Slot (Self.Sys_Time_T_Get, (Store => Arg, Header => Self.Get_Slot_Header (Arg.Slot))));

                                 -- Finally output a new summary packet:
                                 Ignore_Stat := Self.Dump_Summary;
                              end;
                           end if;
                        end;
                     end if;
                  end;
            end case;
         end;
      end if;

      -- Release memory region:
      Self.Sequence_Store_Memory_Region_Release_T_Send ((Sequence_Region => Arg.Sequence_Region, Status => Status_To_Return));
   end Sequence_Store_Memory_Region_Store_T_Recv_Async;

   -- This connector is used to fetch a pointer to a sequence found in the store given its ID.
   overriding function Sequence_Store_Memory_Region_Fetch_T_Service (Self : in out Instance; Arg : in Packed_Sequence_Id.T) return Sequence_Store_Memory_Region_Fetch.T is
      use Sequence_Store_Enums.Sequence_Fetch_Status;
      Slot : Sequence_Store_Types.Slot_Number;
      -- Locate an active sequence with this ID.
      Status : constant Find_Status := Self.Active_Sequence_List.Find_Sequence (Id => Arg.Id, Slot => Slot);
   begin
      -- Check the status:
      case Status is
         when Success =>
            -- Form the sequence memory region for the correct slot.
            return (Sequence_Region => Self.Get_Sequence_Memory_Region (Slot_Index => Slot), Status => Success);
         when Id_Not_Found =>
            -- ID, not found, return the correct status.
            return (Sequence_Region => (Address => System.Null_Address, Length => 0), Status => Id_Not_Found);
      end case;
   end Sequence_Store_Memory_Region_Fetch_T_Service;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Command_Dropped (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Sequence_Store_Memory_Region_Store_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Sequence_Store_Memory_Region_Store_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T) is
      use Sequence_Store_Enums.Sequence_Store_Status;
   begin
      -- Release memory region:
      Self.Sequence_Store_Memory_Region_Release_T_Send ((Sequence_Region => Arg.Sequence_Region, Status => Dropped));

      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Region_Store_Dropped (Self.Sys_Time_T_Get, Arg));
   end Sequence_Store_Memory_Region_Store_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Sequence Store component.
   -- Activate a sequence slot so that its contents can be fetched.
   overriding function Activate_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Sequence_Store_Enums.Slot_State_Type;
   begin
      if Self.Is_Slot_Number_In_Range (Arg.Slot) then
         declare
            Slot_Header : Sequence_Store_Slot_Header.T := Self.Get_Slot_Header (Arg.Slot);
         begin
            -- If the sequence is currently active then we need to remove it from the active sequences list.
            case Slot_Header.Slot_Info.State is
               -- Nothing to do, it is already Activated, just throw an event.
               when Active =>
                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Activated_Slot (Self.Sys_Time_T_Get, Arg));
                  return Success;
               when Inactive =>
                  -- We only want to activate this slot if there is not already a slot that is activated that contains
                  -- the same sequence ID. We can never have two sequences of the same ID activated at the same time
                  -- or there is no way to determine which to return when fetched.
                  declare
                     Status : Add_Status;
                  begin
                     Self.Active_Sequence_List.Add_Sequence (Id => Slot_Header.Seq_Header.Id, Slot => Arg.Slot, Status => Status);

                     -- Check the status:
                     case Status is
                        -- All is good.
                        when Success =>
                           -- Mark the slot as Active.
                           Slot_Header.Slot_Info.State := Active;
                           Self.Set_Slot_Header (Arg.Slot, Slot_Header);

                           -- Send info event:
                           Self.Event_T_Send_If_Connected (Self.Events.Activated_Slot (Self.Sys_Time_T_Get, Arg));
                           return Self.Dump_Summary;

                           -- Duplicate ID already activated.
                        when Duplicate_Id =>
                           -- Throw event and fail.
                           Self.Event_T_Send_If_Connected (Self.Events.Cannot_Activate_Duplicate_Sequence_Id (Self.Sys_Time_T_Get, (Id => Slot_Header.Seq_Header.Id)));
                           return Failure;
                     end case;
                  end;
            end case;
         end;
      else
         -- Event already sent:
         return Failure;
      end if;
   end Activate_Slot;

   -- Deactivate a sequence slot so that its contents can no longer be fetched.
   overriding function Deactivate_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Sequence_Store_Enums.Slot_State_Type;
   begin
      if Self.Is_Slot_Number_In_Range (Arg.Slot) then
         declare
            Slot_Header : Sequence_Store_Slot_Header.T := Self.Get_Slot_Header (Arg.Slot);
         begin
            -- If the sequence is currently active then we need to remove it from the active sequences list.
            case Slot_Header.Slot_Info.State is
               when Active =>
                  declare
                     -- We ignore the returned status. If there was no sequence of this ID in the list, then that is OK,
                     -- since we are trying to remove it anyways.
                     Ignore_Status : Find_Status;
                  begin
                     Self.Active_Sequence_List.Remove_Sequence (Id => Slot_Header.Seq_Header.Id, Status => Ignore_Status);
                  end;

                  -- Mark the slot as inactive.
                  Slot_Header.Slot_Info.State := Inactive;
                  Self.Set_Slot_Header (Arg.Slot, Slot_Header);

                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Deactivated_Slot (Self.Sys_Time_T_Get, Arg));
                  return Self.Dump_Summary;

                  -- Nothing to do, it is already deactivated, just throw an event.
               when Inactive =>
                  -- Send info event:
                  Self.Event_T_Send_If_Connected (Self.Events.Deactivated_Slot (Self.Sys_Time_T_Get, Arg));
                  return Success;
            end case;
         end;
      else
         -- Event already sent:
         return Failure;
      end if;
   end Deactivate_Slot;

   -- Check the CRC of a sequence in a particular slot to see if it matches the CRC found in the header.
   overriding function Check_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Is_Slot_Number_In_Range (Arg.Slot) then
         declare
            Valid_Status : constant Sequence_Store_Enums.Slot_Valid_Type.E := Self.Check_Sequence_Store_Slot (Arg.Slot);
         begin
            -- Throw info event:
            Self.Event_T_Send_If_Connected (Self.Events.Checked_Slot_Validity (Self.Sys_Time_T_Get, (Slot => Arg.Slot, Validity => Valid_Status)));

            -- Dump a new summary packet:
            return Self.Dump_Summary;
         end;
      else
         -- Event already sent:
         return Failure;
      end if;
   end Check_Slot;

   -- Check the CRC of sequences in all sequence store slots to see if they match the CRCs found in their headers.
   overriding function Check_All_Slots (Self : in out Instance) return Command_Execution_Status.E is
   begin
      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Checking_All_Slot_Validity (Self.Sys_Time_T_Get));

      -- Check validity for all slots:
      Self.Check_All_Sequence_Store_Slots;

      -- Dump a new summary packet:
      return Self.Dump_Summary;
   end Check_All_Slots;

   -- Produce a packet with the currently storage slot summary information.
   overriding function Dump_Summary (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Dumping_Slot_Summary (Self.Sys_Time_T_Get));

      -- Create the packet and dump it.
      Self.Send_Slot_Summary_Packet;

      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Dumped_Slot_Summary (Self.Sys_Time_T_Get));
      return Success;
   end Dump_Summary;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Sequence_Store.Implementation;
