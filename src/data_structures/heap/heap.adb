with Safe_Deallocator;

package body Heap is
   ----------------------------------
   -- Public sub programs:
   ----------------------------------

   procedure Init (Self : in out Instance; Maximum_Size : in Positive) is
   begin
      -- Declare the heap on the heap :)
      Self.Tree := new Node_Array (Natural'First .. Natural'First + Maximum_Size - 1);
   end Init;

   procedure Destroy (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Node_Array, Name => Node_Array_Access);
   begin
      Self.Clear;
      if Self.Tree /= null then
         Free_If_Testing (Self.Tree);
      end if;
      Self.Order := Interfaces.Unsigned_32'First;
      Self.Count := Natural'First;
      Self.Max_Count := Natural'First;
   end Destroy;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Order := Interfaces.Unsigned_32'First;
      Self.Count := Natural'First;
      Self.Max_Count := Natural'First;
   end Clear;

   --
   -- Small inline functions that are useful and traversing a heap data structure:
   --

   -- Get the parent of index of the current index. Note that this function should
   -- not be called on the top element of the heap which would have an index of zero,
   -- thus the positive type.
   function Get_Parent_Index (Index : in Positive) return Natural is
   begin
      return (Natural (Index) - 1) / 2;
   end Get_Parent_Index;
   pragma Inline (Get_Parent_Index);

   -- Get the left child of the current index. Note that this function can return
   -- and index out of bounds, so the result should be checked by the caller.
   function Get_Left_Child_Index (Index : in Natural) return Natural is
   begin
      return Index * 2 + 1;
   end Get_Left_Child_Index;
   pragma Inline (Get_Left_Child_Index);

   -- Get the left child of the current index. Note that this function can return
   -- and index out of bounds, so the result should be checked by the caller.
   function Get_Right_Child_Index (Index : in Natural) return Natural is
   begin
      return Index * 2 + 2;
   end Get_Right_Child_Index;
   pragma Inline (Get_Right_Child_Index);

   --
   -- Helper function which enforces the heap property:
   --
   -- This is a non-recursive heapify method. It has an additional property of
   -- stability. Items pushed onto the heap of the same priority will be popped
   -- in FIFO order.
   --
   -- Given the heap, this function reorders elements in the heap from the top
   -- to the bottom, making sure that each parent element is greater than or
   -- equal to its children in terms of priority.
   procedure Heapify (Self : in out Instance) is
      --
      -- Some internal helper functions:
      --
      -- Give back the index of the largest node. The input and output are both
      -- indexes into the heap.
      function Max (Index_A : in Natural; Index_B : in Natural) return Natural is
      begin
         pragma Assert (Index_A < Self.Count, "index_A invalid.");
         pragma Assert (Index_B < Self.Count, "index_B invalid.");

         declare
            use Interfaces;
            Node_A : Node renames Self.Tree (Index_A);
            Node_B : Node renames Self.Tree (Index_B);
         begin
            -- If the nodes are equal then return the
            -- the "oldest" node according to the order.
            -- This is the most common case, so we check
            -- it first:
            if Node_A.Element = Node_B.Element then
               if Node_A.Order < Node_B.Order then
                  return Index_A;
               else
                  return Index_B;
               end if;
               -- If they are not equal then return the largest:
            elsif Node_A.Element > Node_B.Element then
               return Index_A;
            else
               return Index_B;
            end if;
         end;
      end Max;

      -- Swap two nodes given their indexes:
      procedure Swap (Index_A : in Natural; Index_B : in Natural) is
      begin
         pragma Assert (Index_A < Self.Count, "index_A invalid.");
         pragma Assert (Index_B < Self.Count, "index_B invalid.");
         declare
            Temp : constant Node := Self.Tree (Index_A);
         begin
            Self.Tree (Index_A) := Self.Tree (Index_B);
            Self.Tree (Index_B) := Temp;
         end;
      end Swap;

      -- Current index we are checking:
      Index : Natural := Self.Tree.all'First;
      -- Keep track of iterations to ensure that we always exit the while loop.
      Iter : constant Natural := 0;
      Max_Iter : constant Natural := Self.Count + 1;
   begin
      -- Start at top of the tree. Look at the children of every node,
      -- swapping the largest child with the parent. Do this until
      -- the entire tree is ordered appropriately.
      while Index <= Self.Count and then Iter < Max_Iter loop
         declare
            -- Get the children indexes for this node:
            Left : constant Natural := Get_Left_Child_Index (Index);
            Right : constant Natural := Get_Right_Child_Index (Index);
            Largest : Natural;
         begin
            -- Make sure the children indexes make sense:
            pragma Assert (Left > Index, "Left child invalid.");
            pragma Assert (Right > Left, "Right child invalid.");

            -- If the left child is larger than or equal to the heap
            -- size then   we have reached the end of the heap, so we can stop.
            if Left >= Self.Count then
               exit;
            end if;

            -- Initialize the largest node to the current node:
            Largest := Index;

            -- Which is larger, the current node of the left node?:
            Largest := Max (Left, @);

            -- Make sure the right node exists before checking it:
            if Right < Self.Count then
               -- Which is larger, the current node of the left node?:
               Largest := Max (Right, @);
            end if;

            -- If the largest node is the current node then we are
            -- all done heapifying:
            if Largest = Index then
               exit;
            else
               -- Otherwise, we need to swap the largest node with
               -- the current node.
               Swap (Index, Largest);

               -- Set the new index to whichever child was larger:
               Index := Largest;
            end if;
         end;
      end loop;

      -- Make sure nothing went awry:
      pragma Assert (Index <= Self.Count, "Index invalid.");
      pragma Assert (Iter < Max_Iter, "We looped too many times.");
   end Heapify;

   --
   -- Public functions:
   --

   function Push (Self : in out Instance; Element : in Element_Type) return Boolean is
      use Interfaces;
      -- Start at the end of the heap, at the first unallocated element.
      Index : Natural := Self.Count;
      -- Keep track of iterations to ensure that we always exit the while loop.
      Iter : Natural := 0;
      Max_Iter : constant Natural := Self.Count + 1;
   begin
      -- If the heap is full, return false:
      if Self.Count >= Self.Tree.all'Length then
         return False;
      end if;

      -- Start at the bottom of the heap and work our ways upwards
      -- until we find a parent that has a value greater than ours:
      while Index > Self.Tree.all'First and then Iter < Max_Iter loop
         declare
            -- Get the parent index of the current index in the heap.
            Parent : constant Natural := Get_Parent_Index (Index);
         begin
            -- The parent index should always be less than the current index
            -- otherwise the heap is broken.
            pragma Assert (Parent < Index, "The parent index should always be less than the current index in a heap.");

            -- If the current element is greater than or equal to the
            -- parent element we just found, then this is the correct
            -- place to insert the current element.
            if Self.Tree (Parent).Element > Element or else Self.Tree (Parent).Element = Element then
               exit;
            else
               -- The current element is less than the parent, so
               -- let's keep going. We swap the parent with the current
               -- element that we need to store above this parent.
               Self.Tree (Index) := Self.Tree (Parent);
               Index := Parent;
            end if;
         end;
         -- Increment our safety valve:
         Iter := @ + 1;
      end loop;

      -- OK, index now contains the correct place to insert our new
      -- element. And we have moved all nodes below it down in order
      -- to make room. Make sure the current index makes sense:
      pragma Assert (Index <= Self.Tree'Last, "Index too large!");
      pragma Assert (Iter < Max_Iter, "We looped too many times.");

      -- Insert the new element at the proper location:
      Self.Tree (Index) := (Order => Self.Order, Element => Element);

      -- Increment the count and the order:
      Self.Count := @ + 1;
      Self.Order := @ + 1;

      -- Update the high water mark if necessary:
      if Self.Count > Self.Max_Count then
         Self.Max_Count := Self.Count;
      end if;

      return True;
   end Push;

   function Pop (Self : in out Instance; Element : out Element_Type) return Boolean is
   begin
      -- First peek the top element:
      if not Self.Peek (Element) then
         return False;
      end if;

      -- If the peek succeeded then we need to reorganize the heap.
      -- Place the last element on the heap in the root
      -- position and resize the heap. This will put the smallest
      -- value in the heap on the top, violating the heap property.
      Self.Count := @ - 1; -- This should never underflow
      -- because of the check above.
      Self.Tree (Self.Tree.all'First) := Self.Tree (Self.Count);

      -- If the heap is now empty, then reset the order. This helps
      -- us avoid roll over of the order variable which would make
      -- the heap temporarily unstable. If the heap is drained before
      -- the order reaches its maximum, the heap will always be stable.
      -- This is just good housekeeping, increasing our chances of never
      -- experiencing roll over.
      if Self.Count = 0 then
         Self.Order := 0;
      else
         -- There are still elements in the heap, but the heap is
         -- violating the heap property, since the smallest priority
         -- node is now at the top. Let's fix this:
         Self.Heapify;
      end if;
      return True;
   end Pop;

   function Peek (Self : in Instance; Element : out Element_Type) return Boolean is
   begin
      -- If the heap is empty, then there is nothing to pop:
      if Self.Count = 0 then
         return False;
      end if;

      -- Set the return value to the top (max) of the heap:
      Element := Self.Tree (Self.Tree.all'First).Element;
      return True;
   end Peek;

   function Get_Size (Self : in Instance) return Natural is
   begin
      return Self.Count;
   end Get_Size;

   function Get_Maximum_Size (Self : in Instance) return Natural is
   begin
      return Self.Max_Count;
   end Get_Maximum_Size;

   function Get_Capacity (Self : in Instance) return Positive is
   begin
      return Self.Tree'Length;
   end Get_Capacity;

   function Is_Full (Self : in Instance) return Boolean is
   begin
      return Self.Count = Self.Tree'Length;
   end Is_Full;

   function Is_Empty (Self : in Instance) return Boolean is
   begin
      return Self.Count = 0;
   end Is_Empty;

end Heap;
