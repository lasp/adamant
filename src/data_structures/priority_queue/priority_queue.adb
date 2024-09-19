with Safe_Deallocator;

package body Priority_Queue is

   procedure Init (Self : in out Instance; Element_Size : in Positive; Depth : in Positive) is
   begin
      -- Allocate memory for the queue elements on the heap:
      Self.Queue := new Queue_Type (0 .. Queue_Index_Type (Depth) - 1);

      -- Allocate memory for each queue element on the heap:
      for Buffer of Self.Queue.all loop
         Buffer := new Queue_Buffer_Type (0 .. Element_Size - 1);
      end loop;

      -- Allocate memory for the index fifo. We need a unique index for every
      -- slot in the queue.
      Self.Index_Pool := new Queue_Buffer_Index_Array (0 .. Depth - 1);
      for Idx in Self.Index_Pool.all'Range loop
         -- Point each index in the fifo to each index in the queue, 0, 1, 2, 3, etc.
         Self.Index_Pool.all (Idx) := Self.Queue.all'First + Queue_Index_Type (Idx) - Queue_Index_Type (Self.Index_Pool.all'First);
      end loop;

      -- Allocate space for the priority heap on the program heap:
      Self.Priority_Heap.Init (Maximum_Size => Depth);
   end Init;

   -- Destroy all bytes on the queue and the internal priority heap:
   procedure Destroy (Self : in out Instance) is
      procedure Free_Queue_Buffer_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Queue_Buffer_Type, Name => Queue_Buffer_Type_Access);
      procedure Free_Queue_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Queue_Type, Name => Queue_Type_Access);
      procedure Free_Index_Array_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Queue_Buffer_Index_Array, Name => Queue_Buffer_Index_Array_Access);
   begin
      Self.Clear;
      if Self.Queue /= null then
         for Buffer of Self.Queue.all loop
            Free_Queue_Buffer_If_Testing (Buffer);
         end loop;
         Free_Queue_If_Testing (Self.Queue);
      end if;
      if Self.Index_Pool /= null then
         Free_Index_Array_If_Testing (Self.Index_Pool);
      end if;
      Self.Priority_Heap.Destroy;
   end Destroy;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Priority_Heap.Clear;
      Self.Index_Pool_Start := Natural'First;
      Self.Index_Pool_Stop := Natural'First;
      Self.Max_Count := Natural'First;
   end Clear;

   function Is_Full (Self : in Instance) return Boolean is
   begin
      return Self.Priority_Heap.Is_Full;
   end Is_Full;

   function Is_Empty (Self : in Instance) return Boolean is
   begin
      return Self.Priority_Heap.Is_Empty;
   end Is_Empty;

   function Get_Count (Self : in Instance) return Natural is
   begin
      return Self.Priority_Heap.Get_Size;
   end Get_Count;

   function Get_Max_Count (Self : in Instance) return Natural is
   begin
      return Self.Priority_Heap.Get_Maximum_Size;
   end Get_Max_Count;

   function Num_Bytes_Free (Self : in Instance) return Natural is
      Capacity : constant Natural := Self.Priority_Heap.Get_Capacity;
      Used : constant Natural := Self.Priority_Heap.Get_Size;
   begin
      if Used >= Capacity then
         return 0;
      else
         return (Capacity - Used) * Self.Queue.all (Self.Queue.all'First)'Length;
      end if;
   end Num_Bytes_Free;

   function Num_Bytes_Used (Self : in Instance) return Natural is
   begin
      return Self.Priority_Heap.Get_Size * Self.Queue.all (Self.Queue.all'First)'Length;
   end Num_Bytes_Used;

   function Max_Num_Bytes_Used (Self : in Instance) return Natural is
   begin
      return Self.Priority_Heap.Get_Maximum_Size * Self.Queue.all (Self.Queue.all'First)'Length;
   end Max_Num_Bytes_Used;

   function Num_Bytes_Total (Self : in Instance) return Natural is
   begin
      return Self.Priority_Heap.Get_Capacity * Self.Queue.all (Self.Queue.all'First)'Length;
   end Num_Bytes_Total;

   function Current_Percent_Used (Self : in Instance) return Basic_Types.Byte is
      Capacity : constant Positive := Self.Priority_Heap.Get_Capacity;
      Used : constant Integer := Integer (Self.Priority_Heap.Get_Size);
   begin
      if Used > Capacity then
         return 100;
      else
         return Basic_Types.Byte ((Used * 100) / Capacity);
      end if;
   exception
      -- Handle out of bounds error for integer, and say queue is full
      when others =>
         return 100;
   end Current_Percent_Used;

   function Max_Percent_Used (Self : in Instance) return Basic_Types.Byte is
      Capacity : constant Positive := Self.Priority_Heap.Get_Capacity;
      Used : constant Integer := Integer (Self.Priority_Heap.Get_Maximum_Size);
   begin
      if Used > Capacity then
         return 100;
      else
         return Basic_Types.Byte ((Used * 100) / Capacity);
      end if;
   exception
      -- Handle out of bounds error for integer, and say queue is full
      when others =>
         return 100;
   end Max_Percent_Used;

   --
   -- Some helper functions to facilitate push and pop:
   --

   procedure Check_Indexes (Self : in Instance) is
      Elements_On_Queue : constant Natural := Self.Priority_Heap.Get_Size;
   begin
      -- Make sure the indexes make sense based on how many elements are currently
      -- enqueued. This logic helps us find any FSW inconsistencies and will also
      -- catch bit flips.
      if Self.Index_Pool_Start > Self.Index_Pool_Stop then
         pragma Assert ((Self.Index_Pool_Start - Self.Index_Pool_Stop) = Elements_On_Queue);
      elsif Self.Index_Pool_Start < Self.Index_Pool_Stop then
         pragma Assert ((Self.Index_Pool.all'Length - Self.Index_Pool_Stop + Self.Index_Pool_Start - Self.Index_Pool.all'First) = Elements_On_Queue);
      else
         pragma Assert (Elements_On_Queue = 0 or else Elements_On_Queue = Self.Priority_Heap.Get_Capacity);
      end if;
   end Check_Indexes;
   pragma Inline (Check_Indexes);

   function Get_Next_Available_Index (Self : in out Instance) return Queue_Index_Type is
      -- Get an available index from the bool:
      To_Return : constant Queue_Index_Type := Self.Index_Pool.all (Self.Index_Pool_Start);
   begin
      -- Make sure everything makes sense here...
      Self.Check_Indexes;

      -- Increment the start index:
      if Self.Index_Pool_Start < Self.Index_Pool.all'Last then
         Self.Index_Pool_Start := @ + 1;
      else
         Self.Index_Pool_Start := Self.Index_Pool.all'First;
      end if;

      return To_Return;
   end Get_Next_Available_Index;

   procedure Return_Index_To_Pool (Self : in out Instance; Index : in Queue_Index_Type) is
   begin
      -- Make sure everything makes sense here...
      Self.Check_Indexes;
      -- Return the index:
      Self.Index_Pool.all (Self.Index_Pool_Stop) := Index;
      -- Increment the stop index:
      if Self.Index_Pool_Stop < Self.Index_Pool.all'Last then
         Self.Index_Pool_Stop := @ + 1;
      else
         Self.Index_Pool_Stop := Self.Index_Pool.all'First;
      end if;
   end Return_Index_To_Pool;

   function Push (Self : in out Instance; Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status is
   begin
      -- Make sure there are enough slots in the queue:
      if Self.Priority_Heap.Is_Full then
         return Full;
      end if;

      -- Make sure the element we are trying to store is not too large:
      if Bytes'Length > Self.Queue.all (Self.Queue'First)'Length then
         return Too_Large;
      end if;

      -- OK, we are good to store this element.
      declare
         -- Request a storage index:
         Index : constant Queue_Index_Type := Self.Get_Next_Available_Index;
         -- Store entry into max heap:
         Ret : constant Boolean := Self.Priority_Heap.Push ((Priority => Priority, Queue_Buffer_Index => Index, Length => Bytes'Length));
      begin
         -- Storage onto heap should not have failed since we already made sure there
         -- was space:
         pragma Assert (Ret, "Heap storage failed.");

         -- Store data onto queue (if there is any):
         if Bytes'Length > 0 then
            Self.Queue.all (Index) (0 .. Bytes'Length - 1) := Bytes;
         end if;
      end;

      return Success;
   end Push;

   function Peek (Self : in Instance; Priority : out Priority_Type; Num_Bytes_Returned : out Natural) return Peek_Status is
      -- Try to peek the element off the heap:
      Element : Heap_Element;
      Ret : constant Boolean := Self.Priority_Heap.Peek (Element);
   begin
      -- Initialize out parameter:
      Num_Bytes_Returned := 0;

      -- Make sure the peek succeeded:
      if not Ret then
         return Empty;
      end if;

      -- The peek went well. Return the info to the caller:
      Priority := Element.Priority;
      Num_Bytes_Returned := Element.Length;
      return Success;
   end Peek;

   function Pop (Self : in out Instance; Priority : out Priority_Type; Num_Bytes_Returned : out Natural; Bytes : in out Basic_Types.Byte_Array) return Pop_Status is
      -- Try to peek the element off the heap:
      Element : Heap_Element;
      Ret : constant Boolean := Self.Priority_Heap.Peek (Element);
   begin
      -- Initialize out parameter:
      Num_Bytes_Returned := 0;

      -- Make sure the peek succeeded:
      if not Ret then
         return Empty;
      end if;

      -- Make sure the caller's byte array is large enough to hold the data:
      if Bytes'Length < Element.Length then
         return Too_Small;
      end if;

      -- Return storage index. This needs to be done before the pop in order
      -- to not invalidate the assertions made within.
      Self.Return_Index_To_Pool (Element.Queue_Buffer_Index);

      -- Ok, now pop the element off the heap.
      declare
         Ignore : Heap_Element;
         Ret2 : constant Boolean := Self.Priority_Heap.Pop (Ignore);
      begin
         -- This should always work since the peek worked.
         pragma Assert (Ret2, "Pop failed.");

         -- Copy over the data to the caller (if there is any).
         if Element.Length > 0 then
            Bytes (Bytes'First .. Bytes'First + Element.Length - 1) := Self.Queue.all (Element.Queue_Buffer_Index) (0 .. Element.Length - 1);
         end if;
      end;

      -- The pop went well. Return the info to the caller:
      Priority := Element.Priority;
      Num_Bytes_Returned := Element.Length;

      return Success;
   end Pop;

end Priority_Queue;
