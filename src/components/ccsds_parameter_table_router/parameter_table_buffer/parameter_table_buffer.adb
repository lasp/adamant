with Safe_Deallocator;
with Packed_U16;
with Interfaces;

package body Parameter_Table_Buffer is

   use Basic_Types;
   use Interfaces;
   use Parameter_Types;

   procedure Create (Self : in out Instance; Buffer_Size : in Positive) is
   begin
      pragma Assert (Self.Buffer = null);
      Self.Buffer := new Basic_Types.Byte_Array (0 .. Buffer_Size - 1);
      Self.Buffer_Index := Self.Buffer'First;
      Self.Table_Id := 0;
      Self.State := Idle;
   end Create;

   procedure Destroy (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Basic_Types.Byte_Array,
         Name => Basic_Types.Byte_Array_Access
      );
   begin
      if Self.Buffer /= null then
         Free_If_Testing (Self.Buffer);
      end if;
      Self.Buffer_Index := 0;
      Self.State := Idle;
      Self.Buffer := null;
   end Destroy;

   function Append (
      Self : in out Instance;
      Data : in Basic_Types.Byte_Array;
      Sequence_Flag : in Ccsds_Enums.Ccsds_Sequence_Flag.E
   ) return Append_Status is
      use Ccsds_Enums.Ccsds_Sequence_Flag;

      -- Try to append bytes to the buffer at the current index.
      -- Returns True on success, False if the data would overflow the buffer.
      -- On success, increments the buffer index and packet counter.
      function Try_Append (Bytes : in Basic_Types.Byte_Array) return Boolean is
      begin
         if Bytes'Length > 0 then
            declare
               Last_Index : constant Natural := Self.Buffer_Index + Bytes'Length - 1;
            begin
               -- Compare would-be last index against buffer's last valid index:
               if Last_Index > Self.Buffer.all'Last then
                  return False;
               end if;
               Self.Buffer (Self.Buffer_Index .. Last_Index) := Bytes;
               Self.Buffer_Index := @ + Bytes'Length;
            end;
         end if;
         Self.Packet_Count := @ + 1;
         return True;
      end Try_Append;

      -- Process a first segment — reset buffer, extract Table ID,
      -- store payload. Returns New_Table on success, or an error status
      -- (Too_Small_Table, Buffer_Overflow) on failure.
      function Process_First_Segment return Append_Status is
      begin
         Self.Buffer_Index := Self.Buffer'First;
         Self.Packet_Count := 0;

         if Data'Length < 2 then
            Self.State := Idle;
            return Too_Small_Table;
         end if;

         -- Extract Table ID from first 2 bytes using packed deserialization:
         declare
            Id_Packed : constant Packed_U16.T := Packed_U16.Serialization.From_Byte_Array (Data (Data'First .. Data'First + 1));
         begin
            -- Ensure that all Packed_U16 values fit within Parameter_Table_Id range. This should be
            -- OK by design, but let's make sure at compile time.
            pragma Warnings (Off, "condition is always False");
            pragma Compile_Time_Error (
               Natural (Unsigned_16'Last) > Natural (Parameter_Table_Id'Last),
               "Packed_U16 range exceeds Parameter_Types.Parameter_Table_Id upper range."
            );
            pragma Compile_Time_Error (
               Natural (Unsigned_16'First) < Natural (Parameter_Table_Id'First),
               "Packed_U16 range exceeds Parameter_Types.Parameter_Table_Id lower range."
            );
            pragma Warnings (On, "condition is always False");
            Self.Table_Id := Parameter_Types.Parameter_Table_Id (Id_Packed.Value);
         end;

         -- Store only the data AFTER the 2-byte Table ID:
         if not Try_Append (Data (Data'First + 2 .. Data'Last)) then
            Self.State := Idle;
            return Buffer_Overflow;
         end if;
         return New_Table;
      end Process_First_Segment;

      Result : Append_Status;
   begin
      -- Buffer must be allocated before appending:
      pragma Assert (Self.Buffer /= null);

      case Sequence_Flag is
         when Unsegmented =>
            -- Treat as a combined FirstSegment + LastSegment. The entire
            -- table is contained in this single packet:
            Self.State := Idle;
            Result := Process_First_Segment;
            if Result = New_Table then
               -- In this case a new table is a complete table since there
               -- is only a single segment to hold the entire table.
               return Complete_Table;
            else
               return Result;
            end if;

         when Firstsegment =>
            Self.State := Receiving_Table;
            return Process_First_Segment;

         when Continuationsegment =>
            if Self.State = Idle then
               return Packet_Ignored;
            end if;

            if not Try_Append (Data) then
               return Buffer_Overflow;
            end if;
            return Buffering_Table;

         when Lastsegment =>
            if Self.State = Idle then
               return Packet_Ignored;
            end if;

            if not Try_Append (Data) then
               return Buffer_Overflow;
            end if;
            Self.State := Idle;
            return Complete_Table;
      end case;
   end Append;

   function Get_Table_Region (Self : in Instance) return Memory_Region.T is
   begin
      pragma Assert (Self.Buffer /= null);
      return (
         Address => Self.Buffer (Self.Buffer'First)'Address,
         Length => Self.Buffer_Index - Self.Buffer'First
      );
   end Get_Table_Region;

   function Get_Full_Buffer_Region (Self : in Instance) return Memory_Region.T is
   begin
      pragma Assert (Self.Buffer /= null);
      return (
         Address => Self.Buffer (Self.Buffer'First)'Address,
         Length => Self.Buffer.all'Length
      );
   end Get_Full_Buffer_Region;

   function Get_Table_Id (Self : in Instance) return Parameter_Types.Parameter_Table_Id is
   begin
      return Self.Table_Id;
   end Get_Table_Id;

   function Get_Table_Length (Self : in Instance) return Natural is
   begin
      if Self.Buffer = null then
         return 0;
      end if;
      return Self.Buffer_Index - Self.Buffer'First;
   end Get_Table_Length;

   function Get_Packet_Count (Self : in Instance) return Natural is
   begin
      return Self.Packet_Count;
   end Get_Packet_Count;

end Parameter_Table_Buffer;
