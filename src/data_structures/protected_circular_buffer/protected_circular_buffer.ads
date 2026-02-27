with Circular_Buffer;
with Basic_Types;
with Serializer_Types;

-- This is a protected (thread safe) buffer that supports the pushing,
-- popping, and peeking of variable sized byte arrays or typed buffer
-- elements.
package Protected_Circular_Buffer is

   -- The variable buffer class instance type:
   type Instance is tagged limited private;

   -- Status types:
   type Push_Status is (Success, Too_Full);
   type Pop_Status is (Success, Empty);
   type Push_Variable_Length_Type_Status is (Success, Too_Full, Serialization_Failure);

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
   -- Add/remove/look at bytes on the buffer, non-blocking operations.
   --

   -- Push data from a byte array onto the buffer. If not enough space remains on the internal buffer to read
   -- store the entire byte array then Too_Full is returned.
   function Push (Self : in out Instance; Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean := False) return Push_Status
      with Inline => True;
      -- Pop data from buffer onto a byte array. The number of bytes returned will match the length
      -- of "bytes". If "bytes" cannot be completely filled then Failure is returned.
   function Pop (Self : in out Instance; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural) return Pop_Status
      with Inline => True;
      -- Peek data from buffer onto a byte array. This function is like pop, except the bytes are not actually
      -- removed from the internal buffer.
   function Peek (Self : in Instance; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural := 0) return Pop_Status
      with Inline => True;

      --
      -- Typed push functions.
      --
      -- These generic functions operate the same as "Push" above but they take a type, call and its serialization function as the bytes
      -- are stored on the internal buffer. These functions are designed such that only a single copy of the data is made during the push,
      -- the copy from the type to the buffer.
      --
      -- Standard push function for statically sized packed types. Pass in the type and serializer function to
      -- instantiate the generic function.
   generic
      type T is private;
   function Push_Type (Self : in out Instance; Src : in T; Overwrite : in Boolean := False) return Push_Status;

   -- Push function for variable sized packed types. Pass in the type, serializer function, and a function which returns
   -- the serialized length of the type when serialized to instantiate the generic function.
   generic
      type T is private;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function Push_Variable_Length_Type (Self : in out Instance; Src : in T; Overwrite : in Boolean := False) return Push_Variable_Length_Type_Status;

   --
   -- Meta data functions:
   --
   -- How many bytes are not currently being used on the pool?
   -- Note: This should be used as information only. If there is 10 bytes free and you need to put 10 bytes on the buffer, this function
   -- does not guarantee that a push will succeed due to overhead use on the buffer.
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

private

   -- Protected buffer type to provide mutual exclusion of internal buffer:
   protected type Protected_Buffer is
      -- Functions that provide read-only access to the private data:
      function Num_Bytes_Free return Natural;
      function Num_Bytes_Used return Natural;
      function Max_Num_Bytes_Used return Natural;
      function Num_Bytes_Total return Natural;
      function Peek (Bytes : out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural) return Pop_Status;
      -- Procedures requiring full mutual exclusion:
      procedure Init (Size : in Natural);
      procedure Init (Bytes : in Basic_Types.Byte_Array_Access);
      procedure Destroy;
      procedure Clear;
      procedure Push (Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean; Status : out Push_Status);
      procedure Pop (Bytes : out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Status : out Pop_Status);
   private
      Buffer : Circular_Buffer.Circular;
   end Protected_Buffer;

   -- The buffer instance contains an unprotected buffer type, a binary semaphore for mutual exclusion,
   -- and two suspension objects to provide synchronization (blocking operations) when trying to pop/peek
   -- from an empty buffer or trying to push to a full buffer.
   type Instance is tagged limited record
      Buffer : Protected_Buffer;
   end record;

end Protected_Circular_Buffer;
