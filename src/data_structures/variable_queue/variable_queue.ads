with Circular_Buffer;
with Basic_Types;
with Ada.Synchronous_Task_Control;
with Serializer_Types;

-- This is a protected (thread safe) queue that supports the pushing,
-- popping, and peeking of variable sized byte arrays or typed queue
-- elements.
package Variable_Queue is

   -- The variable queue class instance type:
   type Instance is tagged limited private;

   --
   -- Status types:
   --
   -- There are many status types because the different functions below can fail in different
   -- ways. Instead of encompassing these in a single, bloated type, a type is created for each
   -- so the caller only has to handle the error cases that pertain to the function they are
   -- calling.
   --
   -- Here is a general description of the enumeration literals:
   --
   -- Success   - The operation succeeded
   -- Too_Full - The queue is too full for the push to succeed
   -- Empty      - The queue is empty and no items can be popped from it.
   -- Error      - An unexpected runtime program error occurred. Deeper investigation is required
   --                  if this error is returned.
   -- Serialization_Failure - The type could not be converted to a byte array for storage onto the
   --                                     queue because it is malformed.
   -- Deserialization_Failure - The bytes stored on the queue could not be converted to the destination
   --                                        type because the bytes make the type illegal (Constraint_Error).
   --
   type Push_Status is (Success, Too_Full);
   type Pop_Status is (Success, Empty);
   type Push_Block_Status is (Success, Error);
   type Pop_Block_Status is (Success, Error);
   type Push_Variable_Length_Type_Status is (Success, Too_Full, Serialization_Failure);
   type Push_Variable_Length_Type_Block_Status is (Success, Serialization_Failure, Error);
   type Pop_Type_Status is (Success, Empty, Deserialization_Failure);
   type Pop_Type_Block_Status is (Success, Deserialization_Failure, Error);

   --
   -- Initialization/destruction functions:
   --
   -- Provide a size, and allocate the memory on the heap using malloc.
   procedure Init (Self : in out Instance; Size : in Natural);
   -- Provide a pointer to an already allocated set of bytes.
   procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access);
   -- Destroy all bytes on the pool:
   procedure Destroy (Self : in out Instance);
   -- Clear all allocations on the pool:
   procedure Clear (Self : in out Instance);

   --
   -- Add/remove/look at bytes on the queue, non-blocking operations.
   --
   -- Push data from a byte array onto the queue. If not enough space remains on the internal queue then
   -- the Too_Full status is returned.
   function Push (Self : in out Instance; Bytes : in Basic_Types.Byte_Array) return Push_Status
      with Inline => True;
      -- Pop a single data element from the queue onto a byte array. The bytes variable is filled with data from the queue up to its maximum
      -- length. Any data beyond the maximum length is still removed from the queue, but not returned to the user. No constraint
      -- error will be thrown as a result of calling this function with too few bytes in the in out array. A byte offset can be provided
      -- which will skip the number of bytes specified before reading the remaining into the output byte array. All the bytes stored as
      -- part of that single data element will still be removed from the queue when offset is specified.
   function Pop (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status
      with Inline => True;
   function Pop (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status
      with Inline => True;
      -- Pop an element off the queue, but don't return it to caller:
   function Pop (Self : in out Instance) return Pop_Status;
   -- Peek data from queue onto a byte array. This function is like pop, except the data is not actually
   -- removed from the internal queue. A byte offset can be provided which will skip the number of bytes specified
   -- before reading the remaining into the output byte array. This can be useful if you want to "look ahead" a few
   -- bytes in the data without reading off the whole thing in one shot.
   function Peek (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status
      with Inline => True;
   function Peek (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status
      with Inline => True;
      -- Get the length of the oldest item on the queue without removing it.
   function Peek_Length (Self : in out Instance; Length : out Natural) return Pop_Status
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
   function Push_Block (Self : in out Instance; Bytes : in Basic_Types.Byte_Array) return Push_Block_Status;
   -- Same as "Pop" above but waits until an item is put onto the queue if the queue is currently empty:
   function Pop_Block (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Block_Status;
   function Pop_Block (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Block_Status;
   -- Same as "Peek" above but waits until an item is put onto the queue if the queue is currently empty:
   function Peek_Block (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Block_Status;
   function Peek_Block (Self : in out Instance; Bytes : out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Block_Status;
   -- Same as "Peek_Length" above but waits until an item is put onto the queue if the queue is currently empty:
   function Peek_Length_Block (Self : in out Instance; Length : out Natural) return Pop_Block_Status;

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
   function Push_Type (Self : in out Instance; Src : in T) return Push_Status;

   -- Blocking version of function above:
   generic
      type T is private;
   function Push_Type_Block (Self : in out Instance; Src : in T) return Push_Block_Status;

   -- Push function for variable sized packed types. Pass in the type, serializer function, and a function which returns
   -- the serialized length of the type when serialized to instantiate the generic function.
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Push_Variable_Length_Type (Self : in out Instance; Src : in T) return Push_Variable_Length_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Push_Variable_Length_Type_Block (Self : in out Instance; Src : in T) return Push_Variable_Length_Type_Block_Status;

   --
   -- Typed peek functions.
   --
   -- These generic functions operate the same as "Peek" above but they take a type, call and its deserialization function as the bytes
   -- are stored on the internal queue. These functions are designed such that only a single copy of the data is made during the peek,
   -- the copy from the queue into the type.

   -- Standard peek function for statically sized packed types. Pass in the type and deserializer function to
   -- instantiate the generic function.
   generic
      type T is private;
   function Peek_Type (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
   function Peek_Type_Block (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status;

   -- Peek function for variable length packed types. Pass in the type, deserializer function, and a function
   -- which returns the serialized length of the type in order to instantiate the generic function.
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Peek_Variable_Length_Type (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Peek_Variable_Length_Type_Block (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status;

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
   function Pop_Type (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
   function Pop_Type_Block (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status;

   -- Pop function for variable length packed types. Pass in the type, deserializer function, and a function
   -- which returns the serialized length of the type in order to instantiate the generic function.
   -- from the queue, the internal queue element is still removed from the internal queue. So you can always count
   -- on pop to remove an element from the queue, even if an error is returned, unless of course the queue is empty.
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Pop_Variable_Length_Type (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Status;

   -- Blocking version of function above:
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Pop_Variable_Length_Type_Block (Self : in out Instance; Dest : out T; Offset : in Natural := 0) return Pop_Type_Block_Status;

   --
   -- Meta data functions:
   --
   -- How many bytes are not currently being used on the pool?
   -- Note: This should be used as information only. If there is 10 bytes free and you need to put 10 bytes on the queue, this function
   -- does not guarantee that a push will succeed due to overhead use on the queue.
   function Num_Bytes_Free (Self : in out Instance) return Natural
      with Inline => True;
      -- How many bytes are being used currently on the pool?
      -- Note: This should be used as information only. See the note for Num_Bytes_Free above.
   function Num_Bytes_Used (Self : in out Instance) return Natural
      with Inline => True;
      -- What is the "high water mark" or the maximum number of bytes
      -- ever seen on the pool since instantiation.
      -- Note: This should be used as information only. See the note for Num_Bytes_Free above.
   function Max_Num_Bytes_Used (Self : in out Instance) return Natural
      with Inline => True;
      -- How many bytes have been allocated to the pool?
   function Size_In_Bytes (Self : in out Instance) return Natural
      with Inline => True;
      -- how many allocated buffers are on the pool currently?
   function Num_Elements (Self : in out Instance) return Natural
      with Inline => True;
      -- what is the maximum number of buffers ever seen on the pool since instantiation?
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

   -- Resolve the element storage constant:
   Element_Storage_Overhead : constant Natural := Circular_Buffer.Queue_Element_Storage_Overhead;

   -- Protected queue type to provide mutual exclusion of internal queue:
   protected type Protected_Queue is
      -- Functions that provide read-only access to the private data:
      function Get_Count return Natural;
      function Get_Max_Count return Natural;
      function Num_Bytes_Free return Natural;
      function Num_Bytes_Used return Natural;
      function Max_Num_Bytes_Used return Natural;
      function Num_Bytes_Total return Natural;
      function Current_Percent_Used return Basic_Types.Byte;
      function Max_Percent_Used return Basic_Types.Byte;
      function Peek (Bytes : out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural) return Pop_Status;
      function Peek_Length (Length : out Natural) return Pop_Status;
      -- Procedures requiring full mutual exclusion:
      procedure Init (Size : in Natural);
      procedure Init (Bytes : in Basic_Types.Byte_Array_Access);
      procedure Destroy;
      procedure Clear;
      -- Note: We release the suspension objects inside of the protected object. This allows the scheduler
      -- to immediately pop from a just-pushed queue if the popper is the highest priority task. If we did not
      -- release the suspension objects inside of the protected object, then it is possible for another
      -- task to run push before the other end of the queue has time to respond, even if it is higher
      -- a higher priority task. This design ensures the most efficient use of memory on the queue.
      procedure Push (Bytes : in Basic_Types.Byte_Array; Not_Empty : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Push_Status);
      procedure Pop (Bytes : out Basic_Types.Byte_Array; Not_Full : in out Ada.Synchronous_Task_Control.Suspension_Object; Length : out Natural; Offset : in Natural; Status : out Pop_Status);
   private
      Queue : Circular_Buffer.Queue;
   end Protected_Queue;

   -- The queue instance contains an unprotected queue type, a binary semaphore for mutual exclusion,
   -- and two suspension objects to provide synchronization (blocking operations) when trying to pop/peek
   -- from an empty queue or trying to push to a full queue.
   type Instance is tagged limited record
      Queue : Protected_Queue;
      Not_Full, Not_Empty : Ada.Synchronous_Task_Control.Suspension_Object;
   end record;

end Variable_Queue;
