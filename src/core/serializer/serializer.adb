package body Serializer is

   -- Byte arrays don't have a "scalar storage order" since they are an array of single byte
   -- items. So this warning doesn't apply. We can safely overlay a byte array with any type
   -- no matter the underlying scalar storage order.
   pragma Warnings (Off, "overlay changes scalar storage order");

   procedure To_Byte_Array (Dest : out Byte_Array; Src : in T) is
      Overlay : constant Byte_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      Dest := Overlay;
   end To_Byte_Array;

   function To_Byte_Array (Src : in T) return Byte_Array is
      Dest : constant Byte_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      return Dest;
   end To_Byte_Array;

   function To_Byte_Array_Unchecked (Dest : out Basic_Types.Byte_Array; Src : in T) return Natural is
      Overlay : constant Byte_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      Dest := Overlay;
      return Serialized_Length;
   end To_Byte_Array_Unchecked;

   procedure To_Byte_Array_Unchecked (Dest : out Basic_Types.Byte_Array; Src : in T) is
      Overlay : constant Byte_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      Dest := Overlay;
   end To_Byte_Array_Unchecked;

   function To_Byte_Array_Unchecked (Src : in T) return Basic_Types.Byte_Array is
      Dest : constant Byte_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      return Dest;
   end To_Byte_Array_Unchecked;

   procedure From_Byte_Array (Dest : out T; Src : in Byte_Array) is
      Overlay : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
   begin
      Overlay := Src;
   end From_Byte_Array;

   function From_Byte_Array (Src : in Byte_Array) return T is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
      -- The annotation below is not enough to remove the false positive for some
      -- reason, so just turn of analysis of this function.
      To_Return : T;
      Overlay : Byte_Array with Import, Convention => Ada, Address => To_Return'Address;
   begin
      Overlay := Src;
      return To_Return;
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
   end From_Byte_Array;

   function From_Byte_Array_Unchecked (Dest : out T; Src : in Basic_Types.Byte_Array) return Natural is
      Overlay : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
   begin
      Overlay := Src (Src'First .. Src'First + Byte_Array'Length - 1);
      return Serialized_Length;
   end From_Byte_Array_Unchecked;

   procedure From_Byte_Array_Unchecked (Dest : out T; Src : in Basic_Types.Byte_Array) is
      Overlay : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
   begin
      Overlay := Src (Src'First .. Src'First + Byte_Array'Length - 1);
   end From_Byte_Array_Unchecked;

   function From_Byte_Array_Unchecked (Src : in Basic_Types.Byte_Array) return T is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
      -- The annotation below is not enough to remove the false positive for some
      -- reason, so just turn of analysis of this function.
      To_Return : T;
      Overlay : Byte_Array with Import, Convention => Ada, Address => To_Return'Address;
   begin
      Overlay := Src (Src'First .. Src'First + Byte_Array'Length - 1);
      return To_Return;
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
   end From_Byte_Array_Unchecked;

   pragma Warnings (On, "overlay changes scalar storage order");

end Serializer;
