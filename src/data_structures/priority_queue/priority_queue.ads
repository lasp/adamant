with Basic_Types;
with Heap;

--
-- This is an unprotected priority queue data structure. Insertion and removal
-- time from the queue is O(log n). The priority queue uses an internal stable
-- heap data structure to determine the order items are popped from the queue.
-- Higher priority items leave the queue first. Items of equal priority leave
-- the queue in FIFO order.
--
-- The queue stores both a generic priority type, which can also contain other user
-- meta data. The user of this generic data type must also specify a Greater_Than
-- and Equal_To function to compare priorities. Note that this priority type is
-- often copied during a push or pop operation, so the type should be kept as small
-- as possible.
--
-- Along side the priority type, the queue also stores a byte array of data that can
-- be up to some maximum length defined at init. Internally this data is only copied
-- twice, once during push, and once during pop, for efficiency.
--
-- This data structure is unprotected and has no synchronization mechanism, so should
-- only be used by a single task, or be made protected before used by multiple tasks.
-- See the Protected_Priority_Queue for a synchronized, protected implementation that
-- uses this data structure.
--
generic
   type Priority_Type is private;   -- The type used for storing the priority of elements put on the queue.
   with function Greater_Than (Left, Right : in Priority_Type) return Boolean is <>; -- Returns true if the left priority is greater
   with function Equal_To (Left, Right : in Priority_Type) return Boolean is <>; -- Returns true if the priorities are equal
package Priority_Queue is

   -- Basic priority queue definition:
   type Instance is tagged limited private;

   --
   -- Initialization/destruction functions:
   --
   -- Provide the size of each queue element in bytes and the depth of the queue. The amount
   -- of data allocated on the heap to service this queue will be (in bytes):
   --
   --    Queue_Storage = Element_Size*depth + (Priority_Type'Size + Natural'Size)*depth
   --
   procedure Init (Self : in out Instance; Element_Size : in Positive; Depth : in Positive);
   -- Destroy all bytes on the queue and the internal priority heap:
   procedure Destroy (Self : in out Instance);
   procedure Clear (Self : in out Instance);

   --
   -- Meta data functions:
   --
   -- Is the priority queue completely full of data?
   function Is_Full (Self : in Instance) return Boolean
      with Inline => True;
   -- Is the queue completely empty?
   function Is_Empty (Self : in Instance) return Boolean
      with Inline => True;
   -- How many elements are on the queue?:
   function Get_Count (Self : in Instance) return Natural
      with Inline => True;
   -- What is the maximum number of elements ever on the queue?:
   function Get_Max_Count (Self : in Instance) return Natural
      with Inline => True;
   -- How many bytes are not currently being used on the buffer?
   function Num_Bytes_Free (Self : in Instance) return Natural
      with Inline => True;
      -- How many bytes are being used currently on the buffer?
   function Num_Bytes_Used (Self : in Instance) return Natural
      with Inline => True;
   -- This is the "high water mark" or the maximum number of bytes
   -- ever seen on the buffer since instantiation.
   function Max_Num_Bytes_Used (Self : in Instance) return Natural
      with Inline => True;
   -- How many bytes have been allocated to the buffer?
   function Num_Bytes_Total (Self : in Instance) return Natural
      with Inline => True;
   -- Returns a byte with value 0 - 100 of the percentage of the queue
   -- that is currently being used. Num_Bytes_Used/Num_Bytes_Total
   function Current_Percent_Used (Self : in Instance) return Basic_Types.Byte;
   -- Returns a byte with value 0 - 100 of the maximum percentage of the queue
   -- that was used since the queue was instantiated. Max_Num_Bytes_Used/Num_Bytes_Total
   -- "high water mark"
   function Max_Percent_Used (Self : in Instance) return Basic_Types.Byte;

   -- Status type:
   -- Success - The push succeeded.
   -- Full - The queue is full and the push could not be performed.
   -- Too_Large - The element to push is larger than a queue element can hold.
   type Push_Status is (Success, Full, Too_Large);
   -- Success - The pop succeeded.
   -- Empty - The queue is empty and there is nothing to pop
   -- Too_Small - The element to push is larger than the caller provided buffer
   type Pop_Status is (Success, Empty, Too_Small);
   -- Success - The pop succeeded.
   -- Empty - The queue is empty and there is nothing to pop
   type Peek_Status is (Success, Empty);

   -- Push an element onto the priority queue:
   function Push (Self : in out Instance; Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status;
   -- Look at the highest priority element on the queue without removing it. The data is not returned, just the number of bytes.
   function Peek (Self : in Instance; Priority : out Priority_Type; Num_Bytes_Returned : out Natural) return Peek_Status;
   -- Pop the highest priority element off the queue and return the data in a caller provided buffer.
   function Pop (Self : in out Instance; Priority : out Priority_Type; Num_Bytes_Returned : out Natural; Bytes : in out Basic_Types.Byte_Array) return Pop_Status;

   -- Declare constant for size of overhead for storing length on the buffer
   -- itself (in bytes):
   Queue_Element_Storage_Overhead : constant Natural;

private

   -- The queue is an array of byte array accesses that will be
   -- allocated on the program heap.
   subtype Queue_Buffer_Type is Basic_Types.Byte_Array;
   subtype Queue_Buffer_Type_Access is Basic_Types.Byte_Array_Access;
   type Queue_Index_Type is new Natural;
   type Queue_Type is array (Queue_Index_Type range <>) of Queue_Buffer_Type_Access;
   type Queue_Type_Access is access all Queue_Type;

   -- The type of element that will be stored on the priority heap,
   -- used to make sure this functions as a priority queue.
   type Heap_Element is record
      Priority : Priority_Type;                     -- Determines element's priority
      Queue_Buffer_Index : Queue_Index_Type; -- Used to reference element's data on queue
      Length : Natural;                                 -- The length of the element's data on queue
   end record;

   -- Define the heap priority comparison functions:
   function Element_Greater_Than (A, B : in Heap_Element) return Boolean is (Greater_Than (A.Priority, B.Priority))
      with Inline => True;
   function Element_Equal_To (A, B : in Heap_Element) return Boolean is (Equal_To (A.Priority, B.Priority))
      with Inline => True;

      -- Define the generic the heap package:
   package Priority_Heap_Package is new Heap (Heap_Element, Element_Greater_Than, Element_Equal_To);

   -- Define internal index array type to keep track of how to assign queue buffer
   -- indexes to new heap elements.
   type Queue_Buffer_Index_Array is array (Natural range <>) of Queue_Index_Type;
   type Queue_Buffer_Index_Array_Access is access all Queue_Buffer_Index_Array;

   -- Internal types for managing the memory pool:
   type Instance is tagged limited record
      Queue : Queue_Type_Access := null;                              -- Stores queue data
      Priority_Heap : Priority_Heap_Package.Instance;          -- Enqueue and dequeue order
      Index_Pool : Queue_Buffer_Index_Array_Access := null; -- Manages available slots in queue
      Index_Pool_Start : Natural := Natural'First;               -- Tracks next available index in queue
      Index_Pool_Stop : Natural := Natural'First;                -- Tracks last used index in queue
      Max_Count : Natural := Natural'First;
   end record;

   -- Resolve the element storage constant:
   Queue_Element_Storage_Overhead : constant Natural := 0;

end Priority_Queue;
