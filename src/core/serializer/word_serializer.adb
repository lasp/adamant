package body Word_Serializer is

   -- Word arrays don't have a "scalar storage order" issue when overlaying with
   -- a type that has compatible alignment, similar to byte arrays. However, we
   -- may still get warnings when overlaying types with different scalar storage
   -- orders, so we disable them here.
   pragma Warnings (Off, "overlay changes scalar storage order");

   --
   -- Native endianness Word_Array conversions:
   --

   procedure To_Word_Array (Dest : out Word_Array; Src : in T) is
      Overlay : constant Word_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      Dest := Overlay;
   end To_Word_Array;

   function To_Word_Array (Src : in T) return Word_Array is
      Dest : constant Word_Array with Import, Convention => Ada, Address => Src'Address;
   begin
      return Dest;
   end To_Word_Array;

   procedure From_Word_Array (Dest : out T; Src : in Word_Array) is
      Overlay : Word_Array with Import, Convention => Ada, Address => Dest'Address;
   begin
      Overlay := Src;
   end From_Word_Array;

   function From_Word_Array (Src : in Word_Array) return T is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
      -- The annotation below is not enough to remove the false positive for some
      -- reason, so just turn off analysis of this function.
      To_Return : T;
      Overlay : Word_Array with Import, Convention => Ada, Address => To_Return'Address;
   begin
      Overlay := Src;
      return To_Return;
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
   end From_Word_Array;

   --
   -- Little-endian Word_Array_Le conversions:
   --

   procedure To_Word_Array_Le (Dest : out Word_Array_Le; Src : in T) is
      Overlay : constant Word_Array_Le with Import, Convention => Ada, Address => Src'Address;
   begin
      Dest := Overlay;
   end To_Word_Array_Le;

   function To_Word_Array_Le (Src : in T) return Word_Array_Le is
      Dest : constant Word_Array_Le with Import, Convention => Ada, Address => Src'Address;
   begin
      return Dest;
   end To_Word_Array_Le;

   procedure From_Word_Array_Le (Dest : out T; Src : in Word_Array_Le) is
      Overlay : Word_Array_Le with Import, Convention => Ada, Address => Dest'Address;
   begin
      Overlay := Src;
   end From_Word_Array_Le;

   function From_Word_Array_Le (Src : in Word_Array_Le) return T is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
      -- The annotation below is not enough to remove the false positive for some
      -- reason, so just turn off analysis of this function.
      To_Return : T;
      Overlay : Word_Array_Le with Import, Convention => Ada, Address => To_Return'Address;
   begin
      Overlay := Src;
      return To_Return;
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
   end From_Word_Array_Le;

   --
   -- Big-endian Word_Array_Be conversions:
   --

   procedure To_Word_Array_Be (Dest : out Word_Array_Be; Src : in T) is
      Overlay : constant Word_Array_Be with Import, Convention => Ada, Address => Src'Address;
   begin
      Dest := Overlay;
   end To_Word_Array_Be;

   function To_Word_Array_Be (Src : in T) return Word_Array_Be is
      Dest : constant Word_Array_Be with Import, Convention => Ada, Address => Src'Address;
   begin
      return Dest;
   end To_Word_Array_Be;

   procedure From_Word_Array_Be (Dest : out T; Src : in Word_Array_Be) is
      Overlay : Word_Array_Be with Import, Convention => Ada, Address => Dest'Address;
   begin
      Overlay := Src;
   end From_Word_Array_Be;

   function From_Word_Array_Be (Src : in Word_Array_Be) return T is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
      -- The annotation below is not enough to remove the false positive for some
      -- reason, so just turn off analysis of this function.
      To_Return : T;
      Overlay : Word_Array_Be with Import, Convention => Ada, Address => To_Return'Address;
   begin
      Overlay := Src;
      return To_Return;
      pragma Annotate (GNATSAS, False_Positive, "validity check", "to_Return is initialized via overlay copy");
   end From_Word_Array_Be;

   pragma Warnings (On, "overlay changes scalar storage order");

end Word_Serializer;
