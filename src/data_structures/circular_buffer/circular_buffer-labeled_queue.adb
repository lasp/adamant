with Serializer;

package body Circular_Buffer.Labeled_Queue is

   --
   -- Subprograms for Labeled Queue:
   --

   function Push (Self : in out Instance; Label : in Label_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status is
      Stat : Push_Status := Queue_Base (Self).Push_Length (Label_Serializer.Serialized_Length + Bytes'Length);
   begin
      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      declare
         -- Optimization: create a byte array that overlays the label variable then
         -- pass this byte array into the push function to get filled with the length.
         -- This avoids a double copy of the length data:
         Label_Bytes : Label_Serializer.Byte_Array with Import, Convention => Ada, Address => Label'Address;
      begin
         -- Push the label onto the buffer:
         Stat := Base (Self).Push (Label_Bytes);
         pragma Assert (Stat = Success, "Pushing label failed. This can only be false if there is a software bug.");
      end;

      -- Push the data bytes onto buffer:
      Stat := Base (Self).Push (Bytes);
      pragma Assert (Stat = Success, "Pushing bytes failed. This can only be false if there is a software bug.");

      return Success;
   end Push;

   function Do_Peek_Label (Self : in Instance; Label : out Label_Type; Length : out Natural; Element_Length : out Natural) return Pop_Status is
      -- Peek the length:
      Stat : Pop_Status := Queue_Base (Self).Peek_Length (Length);
   begin
      -- Initialize element length to zero:
      Element_Length := 0;

      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      -- Save element length, since we are going to modify "length" to
      -- correspond to the length of the returned data, not the length
      -- of the actual item on the queue:
      Element_Length := Length;

      -- The returned length must be at least as big as the label otherwise there is a bug:
      pragma Assert (Length >= Label_Serializer.Serialized_Length, "Peeking length too small for label. This can only be false is there is a software bug.");

      declare
         Num_Bytes_Returned : Natural;
         -- Optimization: create a byte array that overlays the label variable then
         -- pass this byte array into the peek function to get filled with the label.
         -- This avoids a double copy of the label data:
         Label_Bytes : Label_Serializer.Byte_Array with Import, Convention => Ada, Address => Label'Address;
      begin
         -- Deserialize the label from the buffer:
         Stat := Base (Self).Peek (Label_Bytes, Num_Bytes_Returned, Offset => Length_Serializer.Serialized_Length);
         pragma Assert (Stat = Success, "Peeking label failed. This can only be false if there is a software bug.");
         pragma Assert (Num_Bytes_Returned = Label_Serializer.Serialized_Length, "Peeking label returned too few bytes. This can only be false if there is a software bug.");
      end;

      return Success;
   end Do_Peek_Label;

   function Do_Peek (Self : in Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Element_Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      -- Peek the label:
      Stat : constant Pop_Status := Do_Peek_Label (Self, Label, Length, Element_Length);
   begin
      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      -- Read the bytes from the queue:
      if Element_Length > 0 then
         Queue_Base (Self).Peek_Bytes (Bytes, Element_Length, Length, Label_Serializer.Serialized_Length + Offset);
      end if;

      return Success;
   end Do_Peek;

   function Peek (Self : in Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      Ignore : Natural;
   begin
      return Do_Peek (Self, Label, Bytes, Length, Ignore, Offset);
   end Peek;

   function Peek (Self : in Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status is
      Ignore : Natural;
   begin
      return Self.Peek (Label, Bytes, Ignore, Offset);
   end Peek;

   function Pop (Self : in out Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status is
      -- Peek some bytes:
      Element_Length : Natural;
      Stat : constant Pop_Status := Do_Peek (Self, Label, Bytes, Length, Element_Length, Offset);
   begin
      -- Check return status:
      if Stat /= Success then
         return Stat;
      end if;

      -- Pop the bytes from the base:
      Queue_Base (Self).Do_Pop (Element_Length);

      return Success;
   end Pop;

   function Pop (Self : in out Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status is
      Ignore : Natural;
   begin
      return Self.Pop (Label, Bytes, Ignore, Offset);
   end Pop;

   function Peek_Label (Self : in Instance; Label : out Label_Type) return Pop_Status is
      Ignore1, Ignore2 : Natural;
   begin
      return Do_Peek_Label (Self, Label, Ignore1, Ignore2);
   end Peek_Label;

   overriding function Peek_Length (Self : in Instance; Length : out Natural) return Pop_Status is
      Stat : Pop_Status;
      Total_Length : Natural;
   begin
      -- Initialize to zero:
      Length := 0;

      -- Call the base class length:
      Stat := Queue_Base (Self).Peek_Length (Total_Length);
      if Stat /= Success then
         return Stat;
      end if;

      -- Correct the length for the length of the label:
      Length := Total_Length - Label_Serializer.Serialized_Length;

      return Success;
   end Peek_Length;

end Circular_Buffer.Labeled_Queue;
