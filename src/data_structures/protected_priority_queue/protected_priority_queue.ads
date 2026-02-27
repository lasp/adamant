with Priority_Queue;
with Ada.Synchronous_Task_Control;
with Serializer_Types;
with Basic_Types;

--
-- This is an protected, synchronized priority queue data structure that supports
-- pushing, popping and peeking of variable sized byte arrays of different
-- priorities. Note that this is simply a protected wrapper with synchronization
-- logic (waiting and signaling) on top of the Priority_Queue package.
--
-- Insertion and removal
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
generic
   type Priority_Type is private;   -- The type used for storing the priority of elements put on the queue.
   with function Priority_Greater_Than (Left, Right : in Priority_Type) return Boolean is <>; -- Returns true if the left priority is greater
   with function Priority_Equal_To (Left, Right : in Priority_Type) return Boolean is <>; -- Returns true if the priorities are equal
package Protected_Priority_Queue is

   -- The variable queue class instance type:
   type Instance is tagged limited private;

   -- Status types:
   type Peek_Status is (Success, Empty);
   type Push_Status is (Success, Full, Too_Large);
   type Pop_Status is (Success, Empty, Too_Small);
   type Push_Block_Status is (Success, Too_Large, Error);
   type Pop_Block_Status is (Success, Too_Small, Error);
   type Peek_Block_Status is (Success, Error);
   type Push_Variable_Length_Type_Status is (Success, Full, Too_Large, Serialization_Failure);
   type Push_Variable_Length_Type_Block_Status is (Success, Too_Large, Serialization_Failure, Error);
   type Pop_Type_Status is (Success, Empty, Too_Small, Deserialization_Failure);
   type Pop_Type_Block_Status is (Success, Too_Small, Deserialization_Failure, Error);

   --
   -- Initialization/destruction functions:
   --
   -- Provide the size of each queue element in bytes and the depth of the queue. The amount
   -- of data allocated on the heap to service this queue will be (in bytes):
   --
   --    Queue_Storage = Element_Size*depth + (Priority_Type'Size + Natural'Size)*depth
   --
   procedure Init (Self : in out Instance; Element_Size : in Positive; Depth : in Positive);
   -- Destroy all bytes on the queue:
   procedure Destroy (Self : in out Instance);
   -- Clear all allocations to the queue:
   procedure Clear (Self : in out Instance);

   --
   -- Add/remove/look at bytes on the queue, non-blocking operations.
   --
   -- Push data from a byte array onto the queue. If not enough space remains on the internal queue then
   -- the Full status is returned.
   function Push (Self : in out Instance; Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status
      with Inline => True;
      -- Pop a single data element from the queue onto a byte array. The bytes variable is filled with data from the queue if enough bytes
      -- are provided, otherwise a too_small status is returned.
   function Pop (Self : in out Instance; Priority : out Priority_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural) return Pop_Status
      with Inline => True;
      -- Get the length of the highest priority item on the queue without removing it.
   function Peek_Length (Self : in out Instance; Priority : out Priority_Type; Length : out Natural) return Peek_Status
      with Inline => True;

      --
      -- Add/remove/look at data on the queue, blocking operations.
      --
      -- Note: The options below will either return Success or Error. Error may
      -- be returned on Ravenscar systems only. Ravenscar has a restriction in that
      -- only a single task may wait on a suspension object at any given time. If this
      -- condition is ever violated a Program_Error is raised. In the functions below
      -- we catch this condition and return the Error status, allowing the user to
      -- handle the condition as they please, usually by either moving on, or trying
      -- again. Proper usage of this queue in Adamant should avoid the possibility of
      -- this condition ever occurring. Good design usually avoids more than one task
      -- simultaneously popping or pushing to the same queue in a blocking manner. Use
      -- the "no_wait" version of these functions above whenever possible.
      --
      -- Push data from a byte array onto the queue. If not enough space remains on the internal queue then
      -- wait until there is space available.
   function Push_Block (Self : in out Instance; Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array) return Push_Block_Status;
   -- Same as "Pop" above but waits until an item is put onto the queue if the queue is currently empty:
   function Pop_Block (Self : in out Instance; Priority : out Priority_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural) return Pop_Block_Status;
   -- Same as "Peek_Length" above but waits until an item is put onto the queue if the queue is currently empty:
   function Peek_Length_Block (Self : in out Instance; Priority : out Priority_Type; Length : out Natural) return Peek_Block_Status;

   --
   -- Typed push functions.
   --
   -- These generic functions operate the same as "Push" above but they take a type, call and its serialization function as the bytes
   -- are stored on the internal queue. These functions are designed such that only a single copy of the data is made during the push,
   -- the copy from the type to the queue.
   --
   -- Standard push function for statically sized packed types. Pass in the type and serializer function to
   -- instantiate the generic function.
   generic
      type T is private;
   function Push_Type (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Status;

   -- Blocking version of function above:
   generic
      type T is private;
   function Push_Type_Block (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Block_Status;

   -- Push function for variable sized packed types. Pass in the type, serializer function, and a function which returns
   -- the serialized length of the type when serialized to instantiate the generic function.
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Push_Variable_Length_Type (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Variable_Length_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Push_Variable_Length_Type_Block (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Variable_Length_Type_Block_Status;

   --
   -- Typed pop functions.
   --
   -- These generic functions operate the same as "Pop" above but they take a type, call and its deserialization function as the bytes
   -- are stored on the internal queue. These functions are designed such that only a single copy of the data is made during the peek,
   -- the copy from the queue into the type.

   -- Standard pop function for statically sized packed types. Pass in the type and deserializer function to
   -- instantiate the generic function. Note: even if a deserialization error is encountered during the popping
   -- from the queue, the internal queue element is still removed from the internal queue. So you can always count
   -- on pop to remove an element from the queue, even if an error is returned, unless of course the queue is empty.
   generic
      type T is private;
   function Pop_Type (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
   function Pop_Type_Block (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Block_Status;

   -- Pop function for variable length packed types. Pass in the type, deserializer function, and a function
   -- which returns the serialized length of the type in order to instantiate the generic function.
   -- from the queue, the internal queue element is still removed from the internal queue. So you can always count
   -- on pop to remove an element from the queue, even if an error is returned, unless of course the queue is empty.
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Pop_Variable_Length_Type (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Pop_Variable_Length_Type_Block (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Block_Status;

   --
   -- Meta data functions:
   --
   -- How many bytes are not currently being used on the queue?
   -- Note: This should be used as information only. If there is 10 bytes free and you need to put 10 bytes on the queue, this function
   -- does not guarantee that a push will succeed due to overhead use on the queue.
   function Num_Bytes_Free (Self : in out Instance) return Natural
      with Inline => True;
      -- How many bytes are being used currently on the queue?
      -- Note: This should be used as information only. See the note for Num_Bytes_Free above.
   function Num_Bytes_Used (Self : in out Instance) return Natural
      with Inline => True;
      -- What is the "high water mark" or the maximum number of bytes
      -- ever seen on the queue since instantiation.
      -- Note: This should be used as information only. See the note for Num_Bytes_Free above.
   function Max_Num_Bytes_Used (Self : in out Instance) return Natural
      with Inline => True;
      -- How many bytes have been allocated to the queue?
   function Size_In_Bytes (Self : in out Instance) return Natural
      with Inline => True;
      -- how many allocated buffers are on the queue currently?
   function Num_Elements (Self : in out Instance) return Natural
      with Inline => True;
      -- what is the maximum number of buffers ever seen on the queue since instantiation?
   function Max_Num_Elements (Self : in out Instance) return Natural
      with Inline => True;
      -- Returns a byte with value 0 - 100 of the percentage of the queue
      -- that is currently being used. Num_Bytes_Used/Num_Bytes_Total
   function Current_Percent_Used (Self : in out Instance) return Basic_Types.Byte
      with Inline => True;
      -- Returns a byte with value 0 - 100 of the maximum percentage of the queue
      -- that was used since the queue was instantiated. Max_Num_Bytes_Used/Num_Bytes_Total
      -- "high water mark"
   function Max_Percent_Used (Self : in out Instance) return Basic_Types.Byte
      with Inline => True;

      -- Declare constant for size of overhead for storing length on the buffer
      -- itself (in bytes):
   Element_Storage_Overhead : constant Natural;

private

   -- Priority queue type:
   package Priority_Queue_Package is new Priority_Queue (Priority_Type => Priority_Type, Greater_Than => Priority_Greater_Than, Equal_To => Priority_Equal_To);

   -- Resolve the element storage constant:
   Element_Storage_Overhead : constant Natural := Priority_Queue_Package.Queue_Element_Storage_Overhead;

   -- Protected queue type to provide mutual exclusion of internal queue:
   protected type Protected_Priority_Queue_Wrapper is
      -- Functions that provide read-only access to the private data:
      function Get_Count return Natural;
      function Get_Max_Count return Natural;
      function Num_Bytes_Free return Natural;
      function Num_Bytes_Used return Natural;
      function Max_Num_Bytes_Used return Natural;
      function Num_Bytes_Total return Natural;
      function Current_Percent_Used return Basic_Types.Byte;
      function Max_Percent_Used return Basic_Types.Byte;
      function Peek_Length (Priority : out Priority_Type; Length : out Natural) return Peek_Status;
      -- Procedures requiring full mutual exclusion:
      procedure Init (Element_Size : in Positive; Depth : in Positive);
      procedure Destroy;
      procedure Clear;
      -- Note: We release the suspension objects inside of the protected object. This allows the scheduler
      -- to immediately pop from a just-pushed queue if the popper is the highest priority task. If we did not
      -- release the suspension objects inside of the protected object, then it is possible for another
      -- task to run push before the other end of the queue has time to respond, even if it is higher
      -- a higher priority task. This design ensures the most efficient use of memory on the queue.
      procedure Push (Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array; Not_Empty : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Push_Status);
      procedure Pop (Priority : out Priority_Type; Bytes : out Basic_Types.Byte_Array; Not_Full : in out Ada.Synchronous_Task_Control.Suspension_Object; Length : out Natural; Status : out Pop_Status);
   private
      Queue : Priority_Queue_Package.Instance;
   end Protected_Priority_Queue_Wrapper;

   -- The queue instance contains an unprotected priority queue type,
   -- and two suspension objects to provide synchronization (blocking operations) when trying to pop/peek
   -- from an empty queue or trying to push to a full queue.
   type Instance is tagged limited record
      Queue : Protected_Priority_Queue_Wrapper;
      Not_Full, Not_Empty : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end Protected_Priority_Queue;
