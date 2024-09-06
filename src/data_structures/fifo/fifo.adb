with Safe_Deallocator;

package body Fifo is
   -- Public function bodies:
   procedure Init (Self : in out Instance; Depth : in Positive) is
   begin
      Self.Items := new Fifo_Items (Natural'First .. Natural'First + Depth - 1);
   end Init;

   procedure Destroy (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Fifo_Items, Name => Fifo_Items_Access);
   begin
      Free_If_Testing (Self.Items);
   end Destroy;

   function Is_Full (Self : in Instance) return Boolean is
   begin
      return Self.Count = Self.Items'Length;
   end Is_Full;

   function Is_Empty (Self : in Instance) return Boolean is
   begin
      return Self.Count = 0;
   end Is_Empty;

   function Get_Count (Self : in Instance) return Natural is
   begin
      return Self.Count;
   end Get_Count;

   function Get_Max_Count (Self : in Instance) return Natural is
   begin
      return Self.Max_Count;
   end Get_Max_Count;

   function Get_Depth (Self : in Instance) return Positive is
   begin
      return Self.Items'Length;
   end Get_Depth;

   function Push (Self : in out Instance; Value : in T) return Push_Status is
      Index : constant Natural := (Self.Head + Self.Count) mod Self.Items'Length;
   begin
      if Self.Is_Full then
         return Full;
      end if;

      -- Insert value into buffer:
      Self.Items (Index) := Value;

      -- Increment the buffer count:
      Self.Count := @ + 1;

      -- Update high water mark:
      if Self.Count > Self.Max_Count then
         Self.Max_Count := Self.Count;
      end if;

      return Success;
   end Push;

   function Pop (Self : in out Instance; Value : out T) return Pop_Status is
      Stat : constant Pop_Status := Self.Peek (Value);
   begin
      if Stat /= Success then
         return Stat;
      end if;

      -- Increment the start index and count:
      Self.Head := (@ + 1) mod Self.Items'Length;
      Self.Count := @ - 1;
      return Success;
   end Pop;

   function Peek (Self : in Instance; Value : out T) return Pop_Status is
   begin
      if Self.Is_Empty then
         return Empty;
      end if;

      -- Extract value from buffer:
      Value := Self.Items (Self.Head);

      return Success;
   end Peek;

end Fifo;
