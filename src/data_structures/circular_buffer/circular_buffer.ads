with Basic_Types;
with Serializer;
with Circular_Buffer_Meta;
with Byte_Array_Pointer;

package Circular_Buffer is

   -- Status type:
   type Push_Status is (Success, Too_Full);
   type Pop_Status is (Success, Empty);

   -- Basic Buffer definition:
   -- This is a base type for many other data structures which use
   -- a byte array buffer as their central functioning. This type is pretty
   -- useless on its own. It can be instantiated, deleted, and queried for
   -- meta data, but no access to the internal buffer is granted the user.
   -- Look at inheriting packages for better data structures to use. The
   -- purpose of this package is solely to consolidate shared code and thus
   -- ease of implementation and testing of the inheriting data structures.
   type Base is tagged private;

   --
   -- Initialization/destruction functions:
   --
   -- Provide a size, and allocate the memory on the heap using malloc:
   procedure Init (Self : in out Base; Size : in Natural);
   -- Provide a pointer to an already allocated set of bytes:
   procedure Init (Self : in out Base; Bytes : in not null Basic_Types.Byte_Array_Access);
   -- Destroy all bytes on the buffer:
   procedure Destroy (Self : in out Base);
   -- Clear the buffer:
   procedure Clear (Self : in out Base);

   --
   -- Meta data functions:
   --
   -- Is the circular buffer completely full of data?
   function Is_Full (Self : in Base) return Boolean
      with Inline => True;
   -- Is the circular buffer completely empty?
   function Is_Empty (Self : in Base) return Boolean
      with Inline => True;
   -- How many bytes are not currently being used on the buffer?
   function Num_Bytes_Free (Self : in Base) return Natural
      with Inline => True;
   -- How many bytes are being used currently on the buffer?
   function Num_Bytes_Used (Self : in Base) return Natural
      with Inline => True;
   -- This is the "high water mark" or the maximum number of bytes
   -- ever seen on the buffer since instantiation.
   function Max_Num_Bytes_Used (Self : in Base) return Natural
      with Inline => True;
   -- How many bytes have been allocated to the buffer?
   function Num_Bytes_Total (Self : in Base) return Natural
      with Inline => True;
   -- Returns a byte with value 0 - 100 of the percentage of the queue
   -- that is currently being used. Num_Bytes_Used/Num_Bytes_Total
   function Current_Percent_Used (Self : in Base) return Basic_Types.Byte;
   -- Returns a byte with value 0 - 100 of the maximum percentage of the queue
   -- that was used since the queue was instantiated. Max_Num_Bytes_Used/Num_Bytes_Total
   -- "high water mark"
   function Max_Percent_Used (Self : in Base) return Basic_Types.Byte;
   -- Get the meta data for the circular buffer in a convenient packed
   -- type.
   function Get_Meta_Data (Self : in Base) return Circular_Buffer_Meta.T
      with Inline => True;

   --
   -- Dump functions:
   --
   -- These functions dump the internal circular buffer via an array of pointers.
   -- This array of pointers is of length 2, the second pointer accounting for
   -- wrap arounds in the dump request. Sometimes the dump request can be accommodated
   -- using a single pointer, in which case the second pointer will be set to null
   -- with zero length:
   type Pointer_Dump is array (0 .. 1) of Byte_Array_Pointer.Instance;
   -- Dump all data from oldest to newest:
   function Dump (Self : in Base) return Pointer_Dump;
   -- Return the newest n number of bytes.
   function Dump_Newest (Self : in Base; Num_Bytes_To_Dump : in Natural) return Pointer_Dump;
   -- Return the oldest n number of bytes.
   function Dump_Oldest (Self : in Base; Num_Bytes_To_Dump : in Natural) return Pointer_Dump;
   -- Dump all data in memory order.
   function Dump_Memory (Self : in Base) return Pointer_Dump;

   -- Circular buffer type:
   -- This is an unprotected circular buffer data structure which allows the
   -- user to push and pop byte arrays from an internal buffer. The usage of
   -- this data structure by itself is error prone since it provides little
   -- in the way of type protection, sizing of internal elements, etc. However,
   -- more useful data structures can be built on top of this simple base.
   type Circular is new Base with private;

   --
   -- Add/remove/look at data on the buffer:
   --
   -- Push data from a byte array onto the buffer. If not enough space remains on the internal buffer to read
   -- store the entire byte array then Failure is returned.
   function Push (Self : in out Circular; Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean := False) return Push_Status;
   -- Pop data from buffer onto a byte array. The number of bytes returned will match the length
   -- of "bytes". If "bytes" cannot be completely filled then Failure is returned.
   function Pop (Self : in out Circular; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural) return Pop_Status;
   -- Peek data from buffer onto a byte array. This function is like pop, except the bytes are not actually
   -- removed from the internal buffer.
   function Peek (Self : in Circular; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural := 0) return Pop_Status;

   -- Force the buffer to be completely full, with the head at the specified index. This
   -- subprogram is useful if you want to read out the entire buffer from start to finish
   -- of the allocation in memory.
   procedure Make_Full (Self : in out Circular; Head_Index : Natural := 0)
      with Inline => True;

   -- The queue base type:
   -- This type provides a base type that encapsulates functionality for the inheriting
   -- queue classes below:
   type Queue_Base is new Base with private;

   -- Get the length of the oldest item on the queue without removing it.
   function Peek_Length (Self : in Queue_Base; Length : out Natural) return Pop_Status;
   -- Remove an item off the queue, without returning it:
   function Pop (Self : in out Queue_Base) return Pop_Status;

   --
   -- Meta data functions:
   --
   -- how many items are on the queue currently?
   function Get_Count (Self : in Queue_Base) return Natural
      with Inline => True;
   -- what is the maximum number of items ever seen on the queue since instantiation?
   function Get_Max_Count (Self : in Queue_Base) return Natural
      with Inline => True;
   -- Clear the buffer:
   overriding procedure Clear (Self : in out Queue_Base);

   -- The queue type:
   -- This is an unprotected byte array queue data structure which allows the
   -- user to push and pop byte arrays from an internal buffer. This package
   -- extends the Circular Buffer class, storing the length of each byte
   -- buffer along with the byte buffer itself. Adding this feature, the queue
   -- does not treat its internal store as an array of bytes, but as an array
   -- of elements (stored as bytes). The user can then add and remove these
   -- elements safely, without knowing apriori what the size of each element is
   -- on the queue.
   type Queue is new Queue_Base with private;

   -- Add/remove/look at data on the queue:
   --
   -- Push data from a byte array onto the queue. If not enough space remains on the internal queue to read
   -- store the entire byte array then Failure is returned.
   function Push (Self : in out Queue; Bytes : in Basic_Types.Byte_Array) return Push_Status;
   -- Pop data from queue onto a byte array. The number of bytes returned will match the length
   -- of "bytes". If "bytes" cannot be completely filled then Failure is returned.
   function Pop (Self : in out Queue; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status;
   -- Peek data from queue onto a byte array. This function is like pop, except the bytes are not actually
   -- removed from the internal queue.
   function Peek (Self : in Queue; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status;

   -- Declare constant for size of overhead for storing length on the buffer
   -- itself (in bytes):
   Queue_Element_Storage_Overhead : constant Natural;

private

   -- Internal types for managing the memory pool:
   type Base is tagged record
      Bytes : Basic_Types.Byte_Array_Access := null;
      Head : Natural := 0;
      Count : Natural := 0;
      Max_Count : Natural := 0;
      Allocated : Boolean := False;
   end record;

   --
   -- Add/remove/look at data on the base buffer:
   --
   -- Push data from a byte array onto the buffer. If not enough space remains on the internal buffer to read
   -- store the entire byte array then Failure is returned.
   function Push (Self : in out Base; Bytes : in Basic_Types.Byte_Array; Overwrite : in Boolean := False) return Push_Status;
   -- Pop data from buffer onto a byte array. The function attempts to return a number of bytes equal to
   -- the size of the provided "bytes" array. The actual number of bytes returned is returned in the
   -- Num_Bytes_Returned variable.
   function Pop (Self : in out Base; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural) return Pop_Status;
   -- Peek data from buffer onto a byte array. This function is like pop, except the bytes are not actually
   -- removed from the internal buffer. The function attempts to return a number of bytes equal to
   -- the size of the provided "bytes" array. The actual number of bytes returned is returned in the
   -- Num_Bytes_Returned variable. An offset can be provided to peek ahead a certain number of bytes
   -- from the head of the internal circular buffer.
   function Peek (Self : in Base; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_Returned : out Natural; Offset : in Natural := 0) return Pop_Status;

   --
   -- Generic versions of the functions above:
   --

   -- Internal type for circular buffer:
   type Circular is new Base with record
      null; -- Nothing more needed, just adding methods.
   end record;

   -- Internal type for queue buffer:
   type Queue_Base is new Base with record
      Item_Count : Natural := 0;
      Item_Max_Count : Natural := 0;
   end record;

   -- Length serializer package:
   package Length_Serializer is new Serializer (Natural);

   -- Resolve the element storage constant:
   Queue_Element_Storage_Overhead : constant Natural := Length_Serializer.Serialized_Length;

   -- Queue Base private subprograms:
   function Push_Length (Self : in out Queue_Base; Element_Length : in Natural) return Push_Status;
   procedure Peek_Bytes (Self : in Queue_Base; Bytes : in out Basic_Types.Byte_Array; Num_Bytes_To_Read : in Natural; Num_Bytes_Read : out Natural; Offset : in Natural := 0);
   procedure Do_Pop (Self : in out Queue_Base; Element_Length : in Natural);

   -- Internal type for queue:
   type Queue is new Queue_Base with record
      null; -- Nothing more needed, just adding methods.
   end record;

end Circular_Buffer;
