package body Variable_Serializer is

   -- Byte arrays don't have a "scalar storage order" since they are an array of single byte
   -- items. So this warning doesn't apply. We can safely overlay a byte array with any type
   -- no matter the underlying scalar storage order.
   pragma Warnings (Off, "overlay changes scalar storage order");

   function To_Byte_Array (Dest : out Basic_Types.Byte_Array; Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status is
      -- Get the serialized length of the source:
      Stat : constant Serialization_Status := Serialized_Length_T (Src, Num_Bytes_Serialized);
   begin
      -- Make sure source and destination have valid lengths:
      if Stat /= Success then
         return Stat;
         -- Make sure destination buffer is large enough. We don't use 'Length because
         -- We want to be able to handle the case of a null array where 'Last - 'First + 1
         -- might be negative.
      elsif Num_Bytes_Serialized > Dest'Last - Dest'First + 1 then
         return Failure;
      else
         declare
            -- Overlay source type with properly sized byte array:
            subtype Sized_Byte_Array_Index is Natural range 0 .. Num_Bytes_Serialized - 1;
            subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
            Overlay : constant Sized_Byte_Array with Import, Convention => Ada, Address => Src'Address;
         begin
            -- Copy bytes to destination:
            Dest (Dest'First .. Dest'First + Num_Bytes_Serialized - 1) := Overlay;
         end;

         return Stat;
      end if;
   end To_Byte_Array;

   function To_Byte_Array (Dest : out Basic_Types.Byte_Array; Src : in T) return Serialization_Status is
      Ignore : Natural;
   begin
      return To_Byte_Array (Dest, Src, Ignore);
   end To_Byte_Array;

   function From_Byte_Array (Dest : out T; Src : in Basic_Types.Byte_Array; Num_Bytes_Deserialized : out Natural) return Serialization_Status is
      -- Get the serialized length of the destination:
      Stat : constant Serialization_Status := Serialized_Length_Byte_Array (Src, Num_Bytes_Deserialized);
   begin
      -- Make sure source has a valid length:
      if Stat /= Success then
         return Stat;
         -- Make sure destination buffer is large enough. We don't use 'Length because
         -- We want to be able to handle the case of a null array where 'Last - 'First + 1
         -- might be negative.
      elsif Num_Bytes_Deserialized > Src'Last - Src'First + 1 then
         return Failure;
      else
         declare
            -- Overlay destination type with properly sized byte array:
            subtype Sized_Byte_Array_Index is Natural range 0 .. Num_Bytes_Deserialized - 1;
            subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
            Overlay : Sized_Byte_Array with Import, Convention => Ada, Address => Dest'Address;
         begin
            -- Copy bytes from source:
            Overlay := Src (Src'First .. Src'First + Num_Bytes_Deserialized - 1);
         end;

         return Stat;
      end if;
   end From_Byte_Array;

   function From_Byte_Array (Dest : out T; Src : in Basic_Types.Byte_Array) return Serialization_Status is
      Ignore : Natural;
   begin
      return From_Byte_Array (Dest, Src, Ignore);
   end From_Byte_Array;

   pragma Warnings (On, "overlay changes scalar storage order");

end Variable_Serializer;
