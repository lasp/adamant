package body Protected_Circular_Buffer is

   protected body Protected_Buffer is

      procedure Init (Size : in Natural) is
      begin
         Buffer.Init (Size);
      end Init;

      procedure Init (Bytes : in Basic_Types.Byte_Array_Access) is
      begin
         Buffer.Init (Bytes);
      end Init;

      procedure Destroy is
      begin
         Buffer.Destroy;
      end Destroy;

      procedure Clear is
      begin
         Buffer.Clear;
      end Clear;

      function Num_Bytes_Free return Natural is
      begin
         return Buffer.Num_Bytes_Free;
      end Num_Bytes_Free;

      function Num_Bytes_Used return Natural is
      begin
         return Buffer.Num_Bytes_Used;
      end Num_Bytes_Used;

      function Max_Num_Bytes_Used return Natural is
      begin
         return Buffer.Max_Num_Bytes_Used;
      end Max_Num_Bytes_Used;

      function Num_Bytes_Total return Natural is
      begin
         return Buffer.Num_Bytes_Total;
      end Num_Bytes_Total;

      function Peek (Bytes : out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural) return Pop_Status is
         use Circular_Buffer;
      begin
         case Buffer.Peek (Bytes, Num_Bytes_Returned, Offset) is
            when Success =>
               return Success;
            when Empty =>
               return Empty;
         end case;
      end Peek;

      procedure Push (Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean; Status : out Push_Status) is
         use Circular_Buffer;
      begin
         case Buffer.Push (Bytes, Overwrite) is
            when Success =>
               Status := Success;
            when Too_Full =>
               Status := Too_Full;
         end case;
      end Push;

      procedure Pop (Bytes : out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Status : out Pop_Status) is
         use Circular_Buffer;
      begin
         case Buffer.Pop (Bytes, Num_Bytes_Returned) is
            when Success =>
               Status := Success;
            when Empty =>
               Status := Empty;
         end case;
      end Pop;

   end Protected_Buffer;

   procedure Init (Self : in out Instance; Size : in Natural) is
   begin
      Self.Buffer.Init (Size);
   end Init;

   procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access) is
   begin
      Self.Buffer.Init (Bytes);
   end Init;

   procedure Destroy (Self : in out Instance) is
   begin
      Self.Buffer.Destroy;
   end Destroy;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Buffer.Clear;
   end Clear;

   function Push (Self : in out Instance; Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean := False) return Push_Status is
      Status : Push_Status := Success;
   begin
      Self.Buffer.Push (Bytes, Overwrite, Status);
      return Status;
   end Push;

   function Pop (Self : in out Instance; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural) return Pop_Status is
      Status : Pop_Status := Success;
   begin
      Self.Buffer.Pop (Bytes, Num_Bytes_Returned, Status);
      return Status;
   end Pop;

   function Peek (Self : in Instance; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural := 0) return Pop_Status is
   begin
      return Self.Buffer.Peek (Bytes, Num_Bytes_Returned, Offset);
   end Peek;

   function Push_Type (Self : in out Instance; Src : in T; Overwrite : in Boolean := False) return Push_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      Bytes : Byte_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      return Self.Push (Bytes, Overwrite);
   end Push_Type;

   function Push_Variable_Length_Type (Self : in out Instance; Src : in T; Overwrite : in Boolean := False) return Push_Variable_Length_Type_Status is
      use Serializer_Types;
      -- Get the serialized length of the source:
      Num_Bytes_Serialized : Natural;
      Status : constant Serialization_Status := Serialized_Length (Src, Num_Bytes_Serialized);
   begin
      -- Make sure source has a valid length:
      if Status /= Success then
         return Serialization_Failure;
      end if;

      declare
         -- Overlay source type with properly sized byte array:
         subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_Serialized - 1);
         subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
         Bytes : constant Sized_Byte_Array with Import, Convention => Ada, Address => Src'Address;
      begin
         case Self.Push (Bytes, Overwrite) is
            when Success =>
               return Success;
            when Too_Full =>
               return Too_Full;
         end case;
      end;
   end Push_Variable_Length_Type;

   function Num_Bytes_Free (Self : in out Instance) return Natural is
   begin
      return Self.Buffer.Num_Bytes_Free;
   end Num_Bytes_Free;

   function Num_Bytes_Used (Self : in out Instance) return Natural is
   begin
      return Self.Buffer.Num_Bytes_Used;
   end Num_Bytes_Used;

   function Max_Num_Bytes_Used (Self : in out Instance) return Natural is
   begin
      return Self.Buffer.Max_Num_Bytes_Used;
   end Max_Num_Bytes_Used;

   function Size_In_Bytes (Self : in out Instance) return Natural is
   begin
      return Self.Buffer.Num_Bytes_Total;
   end Size_In_Bytes;

end Protected_Circular_Buffer;
