with AUnit.Assertions; use AUnit.Assertions;
with Safe_Deallocator;

package body History is
   -- Public function bodies:
   procedure Init (Self : in out Instance; Depth : Positive) is
   begin
      Self.Buffer := new History_Buffer (Positive'First .. Positive'First + Depth - 1);
   end Init;

   procedure Destroy (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => History_Buffer, Name => History_Buffer_Access);
   begin
      Free_If_Testing (Self.Buffer);
   end Destroy;

   procedure Push (Self : in out Instance; Value : in T) is
      Index : Positive;
   begin
      Assert (not Self.Is_Full, "History is full. You may need to enlarge it for this test.");

      if not Self.Is_Full then
         -- Insert value into buffer:
         Index := Self.Count + 1;
         Self.Buffer (Index) := Value;

         -- Increment the buffer count:
         Self.Count := @ + 1;
      end if;
   end Push;

   function Get (Self : in Instance; Index : Positive) return T is
   begin
      Assert (Index <= Self.Count, "There is no value stored in the history at index " & Positive'Image (Index) & ". You should check the history count before fetching values.");

      -- Extract value from buffer:
      return Self.Buffer (Index);
   end Get;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Count := 0;
   end Clear;

   function Is_Full (Self : in Instance) return Boolean is
   begin
      return (Self.Count = Self.Buffer'Length);
   end Is_Full;

   function Is_Empty (Self : in Instance) return Boolean is
   begin
      return (Self.Count = 0);
   end Is_Empty;

   function Get_Count (Self : in Instance) return Natural is
   begin
      return Self.Count;
   end Get_Count;

   function Get_Depth (Self : in Instance) return Positive is
   begin
      return Self.Buffer'Length;
   end Get_Depth;

end History;
