with Safe_Deallocator;
with Interfaces;

package body Circular_Buffer is

   --
   -- Subprograms for Base
   --

   procedure Init (Self : in out Base; Size : in Natural) is
   begin
      Self.Bytes := new Basic_Types.Byte_Array (0 .. Size - 1);
      Self.Allocated := True;
   end Init;

   procedure Init (Self : in out Base; Bytes : in not null Basic_Types.Byte_Array_Access) is
   begin
      Self.Bytes := Bytes;
      pragma Assert (Self.Bytes.all'First = Natural'First, "Must be zero indexed.");
      pragma Assert (Self.Bytes.all'Last >= Natural'First, "Must be zero indexed.");
      Self.Allocated := False;
   end Init;

   procedure Destroy (Self : in out Base) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Basic_Types.Byte_Array, Name => Basic_Types.Byte_Array_Access);
   begin
      if Self.Allocated then
         Free_If_Testing (Self.Bytes);
      end if;
      Self.Clear;
   end Destroy;

   procedure Clear (Self : in out Base) is
   begin
      Self.Head := 0;
      Self.Count := 0;
   end Clear;

   function Is_Full (Self : in Base) return Boolean is
   begin
      return Self.Count = Self.Bytes'Length;
   end Is_Full;

   function Is_Empty (Self : in Base) return Boolean is
   begin
      return Self.Count = 0;
   end Is_Empty;

   function Num_Bytes_Free (Self : in Base) return Natural is
   begin
      return Self.Bytes'Length - Self.Count;
   end Num_Bytes_Free;

   function Num_Bytes_Used (Self : in Base) return Natural is
   begin
      return Self.Count;
   end Num_Bytes_Used;

   function Max_Num_Bytes_Used (Self : in Base) return Natural is
   begin
      return Self.Max_Count;
   end Max_Num_Bytes_Used;

   function Num_Bytes_Total (Self : in Base) return Natural is
   begin
      return Self.Bytes'Length;
   end Num_Bytes_Total;

   function Current_Percent_Used (Self : in Base) return Basic_Types.Byte is
      use Basic_Types;
   begin
      if Self.Bytes /= null and then Self.Bytes'Length > 0 then
         declare
            Usage : constant Integer := (Self.Count * 100) / Self.Bytes'Length;
         begin
            if Usage < 100 then
               return Basic_Types.Byte (Usage);
            else
               return 100;
            end if;
         end;
      else
         return 0;
      end if;
   exception
      -- Handle out of bounds error for integer, and say queue is full
      when others =>
         return 100;
   end Current_Percent_Used;

   function Max_Percent_Used (Self : in Base) return Basic_Types.Byte is
      use Basic_Types;
   begin
      if Self.Bytes /= null and then Self.Bytes'Length > 0 then
         declare
            Usage : constant Integer := (Self.Max_Count * 100) / Self.Bytes'Length;
         begin
            if Usage < 100 then
               return Basic_Types.Byte (Usage);
            else
               return 100;
            end if;
         end;
      else
         return 0;
      end if;
   exception
      -- Handle out of bounds error for integer, and say queue is full
      when others =>
         return 100;
   end Max_Percent_Used;

   function Get_Meta_Data (Self : in Base) return Circular_Buffer_Meta.T is
   begin
      return (Head => Interfaces.Unsigned_32 (Self.Head), Count => Interfaces.Unsigned_32 (Self.Count), Size => Interfaces.Unsigned_32 (Self.Bytes'Length));
   end Get_Meta_Data;

   function Do_Dump (Self : in Base; Head : in Natural; Tail : in Natural) return Pointer_Dump is
      To_Return : Pointer_Dump;
   begin
      if Self.Count > 0 then
         -- This function assumes head and tail are in range at this point.
         pragma Assert (Head in Self.Bytes'Range, "Caller must ensure head is in range.");
         pragma Assert (Tail in Self.Bytes'Range, "Caller must ensure tail is in range.");
         -- Dump between head and tail, no wrap around:
         if Tail > Head then
            -- Fill the first pointer with the entire memory:
            To_Return (0) := Byte_Array_Pointer.From_Address (Self.Bytes (Head)'Address, Size => Tail - Head);
            To_Return (1) := Byte_Array_Pointer.Null_Pointer;
         else
            -- Fill the first pointer with the memory to the end:
            To_Return (0) := Byte_Array_Pointer.From_Address (Self.Bytes (Head)'Address, Size => Self.Bytes'Last - Head + 1);
            -- Fill the second pointer with the memory at the beginning:
            To_Return (1) := Byte_Array_Pointer.From_Address (Self.Bytes (Self.Bytes'First)'Address, Size => Tail - Self.Bytes'First);
         end if;
      end if;
      return To_Return;
   end Do_Dump;

   function Dump (Self : in Base) return Pointer_Dump is
      Tail : constant Natural := (Self.Head + Self.Count) mod Self.Bytes'Length;
   begin
      return Do_Dump (Self, Self.Head, Tail);
   end Dump;

   function Dump_Newest (Self : in Base; Num_Bytes_To_Dump : in Natural) return Pointer_Dump is
      Bytes_To_Dump : Natural := Num_Bytes_To_Dump;
   begin
      -- Cap the maximum number of bytes to dump at the count:
      if Bytes_To_Dump > Self.Count then
         Bytes_To_Dump := Self.Count;
      end if;

      if Bytes_To_Dump > 0 then
         declare
            -- Calculate new head that starts Num_Bytes_To_Dump in front of the tail:
            New_Head : constant Natural := (Self.Head + (Self.Count - Bytes_To_Dump)) mod Self.Bytes'Length;
            Tail : constant Natural := (Self.Head + Self.Count) mod Self.Bytes'Length;
         begin
            return Do_Dump (Self, New_Head, Tail);
         end;
      end if;

      return [Byte_Array_Pointer.Null_Pointer, Byte_Array_Pointer.Null_Pointer];
   end Dump_Newest;

   function Dump_Oldest (Self : in Base; Num_Bytes_To_Dump : in Natural) return Pointer_Dump is
      Bytes_To_Dump : Natural := Num_Bytes_To_Dump;
   begin
      -- Cap the maximum number of bytes to dump at the count:
      if Bytes_To_Dump > Self.Count then
         Bytes_To_Dump := Self.Count;
      end if;

      if Bytes_To_Dump > 0 then
         declare
            -- Calculate new tail that starts Num_Bytes_To_Dump behind the head:
            Tail : constant Natural := (Self.Head + Self.Count) mod Self.Bytes'Length;
            New_Tail : constant Natural := (Tail - (Self.Count - Bytes_To_Dump)) mod Self.Bytes'Length;
         begin
            return Do_Dump (Self, Self.Head, New_Tail);
         end;
      end if;

      return [Byte_Array_Pointer.Null_Pointer, Byte_Array_Pointer.Null_Pointer];
   end Dump_Oldest;

   function Dump_Memory (Self : in Base) return Pointer_Dump is
      To_Return : Pointer_Dump;
   begin
      -- Fill the first pointer with the entire memory:
      To_Return (0) := Byte_Array_Pointer.From_Address (Self.Bytes (Self.Bytes'First)'Address, Size => Self.Bytes'Length);
      To_Return (1) := Byte_Array_Pointer.Null_Pointer;
      return To_Return;
   end Dump_Memory;

   function Push (Self : in out Base; Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean := False) return Push_Status is
      Tail : constant Natural := (Self.Head + Self.Count) mod Self.Bytes'Length;
      -- Compute negative length to handle empty array gracefully
      Bytes_Length : constant Integer := Bytes'Last - Bytes'First + 1;
      End_Index : constant Integer := Tail + Bytes_Length - 1;
   begin
      pragma Assert (Self.Bytes.all'First = Natural'First, "Must be zero indexed.");
      pragma Assert (Self.Bytes.all'Last >= Natural'First, "Must be zero indexed.");
      pragma Assert (Tail <= Self.Bytes'Last, "This must be True by design.");

      -- There is nothing to copy:
      if Bytes_Length <= 0 then
         return Success;
      end if;

      -- Make there is enough free bytes left:
      if Bytes_Length > Self.Bytes'Length or else (not Overwrite and then Self.Num_Bytes_Free < Bytes_Length) then
         return Too_Full;
      end if;

      -- Set the correct end index if a roll over is going to happen:
      if End_Index > Self.Bytes'Last then
         declare
            -- Calculate lengths of first and second copies:
            First_Copy_Length : constant Positive := Self.Bytes'Last - Tail + 1;
            Second_Copy_Length : constant Positive := Bytes_Length - First_Copy_Length;
         begin
            -- The addition of the two copies should make sense:
            pragma Assert (First_Copy_Length + Second_Copy_Length = Bytes_Length);
            -- Perform first copy to end of internal array:
            Self.Bytes (Tail .. Self.Bytes'Last) := Bytes (Bytes'First .. Bytes'First + First_Copy_Length - 1);
            -- Wrap around and copy bytes at beginning of internal array:
            Self.Bytes (Self.Bytes'First .. Self.Bytes'First + Second_Copy_Length - 1) := Bytes (Bytes'First + First_Copy_Length .. Bytes'Last);
         end;
      else
         -- Perform single copy onto internal array:
         Self.Bytes (Tail .. End_Index) := Bytes;
      end if;

      -- Set the and count:
      Self.Count := @ + Bytes_Length;
      if Overwrite and then Self.Count > Self.Bytes'Length then
         -- We need to move head if an overwrite actually occurs, since we are
         -- basically "popping" old data by doing the overwrite.
         Self.Head := (@ + (Self.Count - Self.Bytes'Length)) mod Self.Bytes'Length;
         -- The count should never be greater than the length of the buffer.
         Self.Count := Self.Bytes'Length;
      end if;

      -- Set the max count:
      if Self.Count > Self.Max_Count then
         Self.Max_Count := Self.Count;
      end if;

      return Success;
   end Push;

   function Peek (Self : in Base; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural := 0) return Pop_Status is
      Current_Head : constant Natural := Self.Head + Offset;
      -- Compute negative length to handle empty array gracefully
      Bytes_Length : constant Integer := Bytes'Last - Bytes'First + 1;
      Num_Bytes_To_Copy : Integer;
      End_Index : Integer;
   begin
      -- Initialized the number of bytes returned to zero:
      Num_Bytes_Returned := 0;

      -- There is nothing to copy:
      if Bytes_Length <= 0 then
         return Success;
      end if;

      -- Return an error if there is no memory to peek on:
      if Self.Count <= Offset then
         return Empty;
      end if;

      -- Calculate the number of bytes we are going to return:
      Num_Bytes_Returned := Bytes_Length;
      if Num_Bytes_Returned + Offset > Self.Count then
         Num_Bytes_Returned := Self.Count - Offset;
      end if;

      -- Calculate the end index:
      End_Index := Current_Head + Num_Bytes_Returned - 1;

      -- Correct the end index if a roll over is going to happen:
      if End_Index > Self.Bytes'Last then
         End_Index := Self.Bytes'Last;
      end if;

      -- Calculate the number of bytes to copy:
      Num_Bytes_To_Copy := End_Index - Current_Head + 1;

      -- Copy bytes over:
      if Num_Bytes_To_Copy > 0 then
         Bytes (Bytes'First .. Bytes'First + Num_Bytes_To_Copy - 1) := Self.Bytes (Current_Head .. End_Index);
      end if;

      -- If the number of bytes copied was not the full amount then
      -- we need to wrap around.
      if Num_Bytes_To_Copy < Num_Bytes_Returned then
         declare
            Num_Bytes_Copied : Integer := Num_Bytes_To_Copy;
            Bytes_Index : Natural;
            Start_Offset : Natural := 0;
         begin
            -- If num bytes copied is negative, then make correction for
            -- wrap around:
            if Num_Bytes_Copied < 0 then
               Num_Bytes_Copied := 0;
               Start_Offset := -1 * Num_Bytes_To_Copy;
            end if;
            -- Do second copy:
            Bytes_Index := Bytes'First + Num_Bytes_Copied;
            Num_Bytes_To_Copy := Num_Bytes_Returned - Num_Bytes_Copied;
            Bytes (Bytes_Index .. Bytes_Index + Num_Bytes_To_Copy - 1) := Self.Bytes (Start_Offset .. Start_Offset + Num_Bytes_To_Copy - 1);
         end;
      end if;

      return Success;
   end Peek;

   function Pop (Self : in out Base; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural) return Pop_Status is
      Stat : constant Pop_Status := Self.Peek (Bytes, Num_Bytes_Returned);
   begin
      if Stat /= Success then
         return Stat;
      end if;

      -- Set the new head and count if we removed any bytes.
      if Self.Bytes'Length > 0 and then Num_Bytes_Returned > 0 then
         Self.Head := (@ + Num_Bytes_Returned) mod Self.Bytes'Length;
         Self.Count := @ - Num_Bytes_Returned;
      end if;

      -- Optimization: set head to zero if count is zero, this
      -- prevents rollover of the buffer as much as possible.
      if Self.Count = 0 then
         Self.Head := 0;
      end if;

      return Success;
   end Pop;

   --
   -- Subprograms for Circular
   --

   overriding function Push (Self : in out Circular; Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean := False) return Push_Status is
   begin
      return Base (Self).Push (Bytes, Overwrite);
   end Push;

   overriding function Pop (Self : in out Circular; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural) return Pop_Status is
   begin
      return Base (Self).Pop (Bytes, Num_Bytes_Returned);
   end Pop;

   overriding function Peek (Self : in Circular; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural := 0) return Pop_Status is
   begin
      return Base (Self).Peek (Bytes, Num_Bytes_Returned, Offset);
   end Peek;

   procedure Make_Full (Self : in out Circular; Head_Index : Natural := 0) is
   begin
      Self.Head := Head_Index mod Self.Bytes'Length;
      Self.Count := Self.Bytes'Length;
   end Make_Full;

   --
   -- Subprograms for Queue Base
   --
   --

   function Peek_Length (Self : in Queue_Base; Length : out Natural) return Pop_Status is
   begin
      -- Initialize length to zero:
      Length := 0;

      -- Make sure there is data on the queue:
      if Self.Item_Count = 0 then
         return Empty;
      end if;

      declare
         Stat : Pop_Status;
         Num_Bytes_Returned : Natural;
         -- Optimization: create a byte array that overlays the length variable then
         -- pass this byte array into the peek function to get filled with the length.
         -- This avoids a double copy of the length data:
         Length_Bytes : Length_Serializer.Byte_Array with Import, Convention => Ada, Address => Length'Address;
      begin
         -- Deserialize the length from the buffer:
         Stat := Base (Self).Peek (Length_Bytes, Num_Bytes_Returned);
         pragma Assert (Stat = Success, "Peeking length failed. This can only be false if there is a software bug.");
         pragma Assert (Num_Bytes_Returned = Length_Serializer.Serialized_Length, "Peeking length returned too few bytes. This can only be false if there is a software bug.");
      end;

      return Success;
   end Peek_Length;

   procedure Do_Pop (Self : in out Queue_Base; Element_Length : in Natural) is
      Bytes_To_Pop : constant Integer := Element_Length + Length_Serializer.Serialized_Length;
      Ignore_Bytes : Basic_Types.Byte_Array (0 .. Bytes_To_Pop - 1);
      Num_Bytes_Popped : Natural;
      Stat : constant Pop_Status := Base (Self).Pop (Ignore_Bytes, Num_Bytes_Popped);
   begin
      pragma Assert (Stat = Success, "Popping bytes failed. This can only be false if there is a software bug.");
      pragma Assert (Num_Bytes_Popped = Bytes_To_Pop, "Popping bytes returned too few bytes. This can only be false if there is a software bug.");

      -- Decrement the counter:
      Self.Item_Count := @ - 1;
   end Do_Pop;

   function Pop (Self : in out Queue_Base) return Pop_Status is
      Element_Length : Natural;
      Stat : constant Pop_Status := Self.Peek_Length (Element_Length);
   begin
      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      Do_Pop (Self, Element_Length);

      return Success;
   end Pop;

   function Push_Length (Self : in out Queue_Base; Element_Length : in Natural) return Push_Status is
      Len : constant Natural := Element_Length + Length_Serializer.Serialized_Length;
      Stat : Push_Status;
   begin
      -- Make sure we can fit the data and the length:
      if Len > Self.Num_Bytes_Free then
         return Too_Full;
      end if;

      declare
         -- Optimization: create a byte array that overlays the length variable then
         -- pass this byte array into the push function to get filled with the length.
         -- This avoids a double copy of the length data:
         Length_Bytes : Length_Serializer.Byte_Array with Import, Convention => Ada, Address => Element_Length'Address;
      begin
         -- Serialized the length onto the buffer:
         Stat := Base (Self).Push (Length_Bytes);
         pragma Assert (Stat = Success, "Pushing length failed. This can only be false if there is a software bug.");
      end;

      -- Increment the counters:
      Self.Item_Count := @ + 1;
      if Self.Item_Count > Self.Item_Max_Count then
         Self.Item_Max_Count := Self.Item_Count;
      end if;

      return Success;
   end Push_Length;

   procedure Peek_Bytes (Self : in Queue_Base; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_To_Read : in Natural; Num_Bytes_Read : out Natural; Offset : in Natural := 0) is
   begin
      -- Initialize bytes read to zero:
      Num_Bytes_Read := 0;

      -- If there are bytes to peek then do that:
      if Num_Bytes_To_Read > Offset then
         declare
            Num_Bytes_Returned : Natural;
            Stat : Pop_Status;
            Bytes_To_Peek : Natural := Num_Bytes_To_Read - Offset;
         begin
            -- Modify bytes to peek if it is longer than caller's byte array:
            if Bytes_To_Peek > Bytes'Length then
               Bytes_To_Peek := Bytes'Length;
            end if;

            -- Deserialize the data from the buffer:
            Stat := Base (Self).Peek (Bytes (Bytes'First .. Bytes'First + Bytes_To_Peek - 1), Num_Bytes_Returned, Offset => Length_Serializer.Serialized_Length + Offset);
            pragma Assert (Stat = Success, "Peeking bytes failed. This can only be false if there is a software bug.");
            pragma Assert (Num_Bytes_Returned = Bytes_To_Peek, "Peeking bytes returned too few bytes. This can only be false if there is a software bug.");

            -- Return the actual number of bytes read to caller:
            Num_Bytes_Read := Bytes_To_Peek;
         end;
      end if;
   end Peek_Bytes;

   function Get_Count (Self : in Queue_Base) return Natural is
   begin
      return Self.Item_Count;
   end Get_Count;

   function Get_Max_Count (Self : in Queue_Base) return Natural is
   begin
      return Self.Item_Max_Count;
   end Get_Max_Count;

   overriding procedure Clear (Self : in out Queue_Base) is
   begin
      -- Call the base class implementation:
      Clear (Base (Self));
      -- Clear the item count:
      Self.Item_Count := 0;
   end Clear;

   --
   -- Subprograms for Queue
   --
   --

   function Push (Self : in out Queue; Bytes : in Basic_Types.Byte_Array) return Push_Status is
      Stat : Push_Status := Queue_Base (Self).Push_Length (Bytes'Length);
   begin
      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      -- Push the data bytes on to buffer:
      Stat := Base (Self).Push (Bytes);
      pragma Assert (Stat = Success, "Pushing bytes failed. This can only be false if there is a software bug.");

      return Success;
   end Push;

   function Do_Peek (Self : in Queue; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Element_Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      Stat : constant Pop_Status := Self.Peek_Length (Length);
   begin
      -- Initialize the element length to zero:
      Element_Length := 0;

      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      -- Save element length, since we are going to modify "length" to
      -- correspond to the length of the returned data, not the length
      -- of the actual item on the queue:
      Element_Length := Length;

      -- Read the bytes from the queue:
      if Element_Length > 0 then
         Queue_Base (Self).Peek_Bytes (Bytes, Element_Length, Length, Offset);
      end if;

      return Success;
   end Do_Peek;

   overriding function Peek (Self : in Queue; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      Ignore : Natural;
   begin
      return Self.Do_Peek (Bytes, Length, Ignore, Offset);
   end Peek;

   function Pop (Self : in out Queue; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      -- Peek some bytes:
      Element_Length : Natural;
      Stat : constant Pop_Status := Self.Do_Peek (Bytes, Length, Element_Length, Offset);
   begin
      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      -- Pop the bytes from the base:
      Queue_Base (Self).Do_Pop (Element_Length);

      return Success;
   end Pop;

end Circular_Buffer;
