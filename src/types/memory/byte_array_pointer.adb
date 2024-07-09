with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;

package body Byte_Array_Pointer is

   use Basic_Types;

   function Address_To_Ptr (Addr : in System.Address) return Unsafe_Byte_Array_Access is
      -- Useful generic package instantiation for complicated type conversion:
      package Address_Converter is new System.Address_To_Access_Conversions (Constrained_Byte_Array);
      function To_Unsafe_Byte_Array_Access is new Ada.Unchecked_Conversion (Address_Converter.Object_Pointer, Unsafe_Byte_Array_Access);
      -- Convert the first byte of the array to a pointer:
      Ptr : constant Unsafe_Byte_Array_Access := To_Unsafe_Byte_Array_Access (Address_Converter.To_Pointer (Addr));
   begin
      return Ptr;
   end Address_To_Ptr;

   -- Procedures for creating / destroying a pointer on the heap
   -- using "new":
   function Create_On_Heap (Size : in Natural; Init_To_Zero : in Boolean := False) return Instance is
      Ptr : Instance := (Address => System.Null_Address, Length => Size);
      Data : Basic_Types.Byte_Array_Access := null;
   begin
      -- Create a constrained Byte array on the heap:
      Data := new Basic_Types.Byte_Array (0 .. (Size - 1));

      if Init_To_Zero then
         Data.all := [others => 0];
      end if;

      -- Convert the Byte array access to a
      -- Constrained_Byte_Array_Access type:
      Ptr.Address := Data.all'Address;

      return Ptr;
   end Create_On_Heap;

   function From_Address (Addr : in System.Address; Size : in Integer) return Instance is
   begin
      return (Address => Addr, Length => Size);
   end From_Address;

   function Null_Pointer return Instance is
      Ptr : constant Instance := (Address => System.Null_Address, Length => -1);
   begin
      return Ptr;
   end Null_Pointer;

   function From_Typed_Access (Src : in T_Access) return Instance is
      -- Overlay a byte array on top of the src, and then create the pointer as normal.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
   begin
      return (Address => Src.all'Address, Length => Serialized_Length);
   end From_Typed_Access;

   function From_Variable_Length_Typed_Access (Self : out Instance; Src : in T_Access) return Serializer_Types.Serialization_Status is
      use Serializer_Types;
      -- Get the serialized length of the source:
      Num_Bytes_Serialized : Natural;
      Stat : constant Serializer_Types.Serialization_Status := Serialized_Length (Src.all, Num_Bytes_Serialized);
   begin
      -- Make sure source has a valid length:
      if Stat /= Success then
         return Stat;
      end if;

      Self := (Address => Src.all'Address, Length => Num_Bytes_Serialized);

      return Stat;
   end From_Variable_Length_Typed_Access;

   procedure Destroy (Self : in out Instance) is
   begin
      -- Reset length and pointer:
      Self.Length := -1;
      Self.Address := System.Null_Address;
   end Destroy;

   function Slice (Self : in Instance; Start_Index : in Natural; End_Index : in Integer := -1) return Instance is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
      Actual_End_Index : Integer := Self.Length - 1;
   begin
      -- Overwrite default end index if one is provided:
      if End_Index >= 0 then
         Actual_End_Index := End_Index;
      end if;
      -- If end index is before the start index, then correct it so the length still
      -- make sense as 0:
      if Actual_End_Index < Start_Index then
         Actual_End_Index := Start_Index - 1;
      end if;
      return (Address => Safe_Byte_Array (Start_Index .. Actual_End_Index)'Address, Length => Actual_End_Index - Start_Index + 1);
   end Slice;

   function Is_Null (Self : in Instance) return Boolean is
   begin
      return Self.Length < 0;
   end Is_Null;

   function Length (Self : in Instance) return Natural is
   begin
      if Self.Length >= 0 then
         return Self.Length;
      else
         return 0;
      end if;
   end Length;

   function Address (Self : in Instance) return System.Address is
   begin
      return Self.Address;
   end Address;

   function Pointer (Self : in Instance) return Basic_Types.Unsafe_Byte_Array_Access is
   begin
      return Address_To_Ptr (Self.Address);
   end Pointer;

   function To_Byte_Array (Self : in Instance) return Basic_Types.Byte_Array is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
   begin
      return Safe_Byte_Array;
   end To_Byte_Array;

   procedure Copy_To (Self : in Instance; Bytes : in Basic_Types.Byte_Array) is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
   begin
      Safe_Byte_Array := Bytes;
   end Copy_To;

   procedure Copy (Destination : in Instance; Source : in Instance) is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Destination.Length - 1);
      Safe_Destination_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Destination.Address;
      Safe_Source_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Source.Address;
   begin
      pragma Assert (Destination.Length = Source.Length, "The lengths of the pointers must match.");
      Safe_Destination_Byte_Array := Safe_Source_Byte_Array;
   end Copy;

   function To_Type (Self : in Instance) return T is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
   begin
      return From_Byte_Array (Safe_Byte_Array);
   end To_Type;

   function To_Variable_Length_Type (Dest : out T; Self : in Instance) return Serializer_Types.Serialization_Status is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
   begin
      return From_Byte_Array (Dest, Safe_Byte_Array);
   end To_Variable_Length_Type;

   procedure Copy_From_Type (Self : in Instance; Src : in T) is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
   begin
      To_Byte_Array (Safe_Byte_Array, Src);
   end Copy_From_Type;

   function Copy_From_Variable_Length_Type (Self : in Instance; Src : in T) return Serializer_Types.Serialization_Status is
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
   begin
      return To_Byte_Array (Safe_Byte_Array, Src);
   end Copy_From_Variable_Length_Type;

end Byte_Array_Pointer;
