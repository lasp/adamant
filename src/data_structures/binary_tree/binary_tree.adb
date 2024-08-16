with Safe_Deallocator;

package body Binary_Tree is
   ----------------------------------
   -- Public sub programs:
   ----------------------------------

   procedure Init (Self : in out Instance; Maximum_Size : in Positive) is
   begin
      Self.Tree := new Element_Array (Positive'First .. Positive'First + Maximum_Size - 1);
   end Init;

   procedure Destroy (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Element_Array, Name => Element_Array_Access);
   begin
      Free_If_Testing (Self.Tree);
      Self.Clear;
   end Destroy;

   -- Add element to tree. This is done in O(n) time where n is the current size of the tree.
   function Add (Self : in out Instance; Element : in Element_Type) return Boolean is
   begin
      -- Make sure tree is not full:
      if Self.Size >= Self.Tree'Last then
         return False;
      end if;

      declare
         Insert_Index : Positive := Self.Size + 1;
         This_Element : Element_Type := Element;
         Next_Element : Element_Type;
      begin
         -- Search linearly for appropriate place to put element:
         for Index in 1 .. Self.Size loop
            if Element < Self.Tree (Index) then
               Insert_Index := Index;
               exit;
            end if;
         end loop;

         -- Insert the element and then then move the remainder
         -- of the list up one index. If the found index is at
         -- the end then this loop will be skipped.
         for Index in Insert_Index .. Self.Size loop
            Next_Element := Self.Tree (Index);
            Self.Tree (Index) := This_Element;
            This_Element := Next_Element;
         end loop;

         -- Increment size:
         Self.Size := Self.Size + 1;

         -- Move the last element into the new last slot.
         pragma Assert (Self.Size >= Self.Tree'First, "Size is greater than or equal to 1 at this point.");
         Self.Tree (Self.Size) := This_Element;
      end;

      return True;
   end Add;

   function Remove (Self : in out Instance; Element_Index : in Positive) return Boolean is
   begin
      -- Make sure index is in tree:
      if Element_Index > Self.Size then
         return False;
      end if;

      -- Starting with the given element index, start moving
      -- every element past this one a single entry to the left
      -- in the array. This will keep the list sorted and
      -- compact.
      for Index in Element_Index .. (Self.Size - 1) loop
         Self.Tree (Index) := Self.Tree (Index + 1);
      end loop;

      -- Decrement size:
      Self.Size := Self.Size - 1;

      return True;
   end Remove;

   -- Search for element in tree. This is done in O(log n) where n is the current size of the tree.
   function Search (Self : in Instance; Element : in Element_Type; Element_Found : out Element_Type; Element_Index : out Positive) return Boolean is
      pragma Annotate (GNATSAS, Intentional, "validity check", "Out parameters element_Found and element_Index will be uninitialized if no element is found, by design.");
      Low_Index : Natural := Self.Tree'First;
      High_Index : Natural := Self.Size;
   begin
      pragma Assert (Self.Size <= Self.Tree'Last - Self.Tree'First + 1);
      -- Perform binary search on sorted list:
      while Low_Index <= High_Index loop
         declare
            Mid_Index : constant Positive := Low_Index + ((High_Index - Low_Index) / 2);
            Current_Element : Element_Type renames Self.Tree (Mid_Index);
         begin
            if Current_Element > Element then
               High_Index := Mid_Index - 1;
            elsif Current_Element < Element then
               Low_Index := Mid_Index + 1;
            else
               Element_Found := Current_Element;
               Element_Index := Mid_Index;
               return True;
            end if;
         end;
      end loop;

      return False;
   end Search;

   function Get (Self : in Instance; Element_Index : in Positive) return Element_Type is
   begin
      return Self.Tree (Element_Index);
   end Get;

   procedure Set (Self : in out Instance; Element_Index : in Positive; Element : in Element_Type) is
   begin
      Self.Tree (Element_Index) := Element;
   end Set;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Size := 0;
   end Clear;

   function Get_Size (Self : in Instance) return Natural is
   begin
      return Self.Size;
   end Get_Size;

   function Get_Capacity (Self : in Instance) return Positive is
   begin
      return Self.Tree'Length;
   end Get_Capacity;

   function Get_First_Index (Self : in Instance) return Positive is
   begin
      -- If empty, then return 1 as the first index. The last will return 0.
      if Self.Size = 0 then
         return 1;
      else
         return Self.Tree'First;
      end if;
   end Get_First_Index;

   function Get_Last_Index (Self : in Instance) return Natural is
   begin
      return Self.Size;
   end Get_Last_Index;

end Binary_Tree;
