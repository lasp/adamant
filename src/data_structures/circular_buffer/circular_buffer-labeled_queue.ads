with Basic_Types;

generic
   type Label_Type is private;
package Circular_Buffer.Labeled_Queue is

   -- The labeled queue type:
   -- This queue is identical to the Queue type above
   -- except that each element is stored with a generic label. The most obvious
   -- use for this label is to store information regarding the type of the element
   -- stored in the queue, such that the correct deserialization method can be called
   -- to decode the data. However, the label can really be any statically sized
   -- type. If you use a variable length type as the label, the maximum size of that
   -- variable length type will be stored, so this is not recommended.
   type Instance is new Queue_Base with private;

   -- Add/remove/look at data on the queue:
   --
   -- Push data from a byte array onto the queue. If not enough space remains on the internal queue to read
   -- store the entire byte array then Failure is returned.
   function Push (Self : in out Instance; Label : in Label_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status;
   -- Pop data from queue onto a byte array. The number of bytes returned will match the length
   -- of "bytes". If "bytes" cannot be completely filled then Failure is returned.
   function Pop (Self : in out Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status;
   function Pop (Self : in out Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status;
   -- Peek data from queue onto a byte array. This function is like pop, except the bytes are not actually
   -- removed from the internal queue.
   function Peek (Self : in Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Length : out Natural; Offset : in Natural := 0) return Pop_Status;
   function Peek (Self : in Instance; Label : out Label_Type; Bytes : in out Basic_Types.Byte_Array; Offset : in Natural := 0) return Pop_Status;

   -- Get the label of the oldest item on the queue without removing it.
   function Peek_Label (Self : in Instance; Label : out Label_Type) return Pop_Status;
   -- Get the length of the oldest item on the queue without removing it. We need to override because we need to subtract the length of the label.
   overriding function Peek_Length (Self : in Instance; Length : out Natural) return Pop_Status;

   -- Declare constant for size of overhead for storing length on the buffer
   -- and the label itself (in bytes):
   Labeled_Queue_Element_Storage_Overhead : constant Natural;

private

   -- The serializer package for the label:
   package Label_Serializer is new Serializer (Label_Type);

   -- Resolve the element storage constant:
   Labeled_Queue_Element_Storage_Overhead : constant Natural := Circular_Buffer.Queue_Element_Storage_Overhead + Label_Serializer.Serialized_Length;

   -- Internal type for labeled queue:
   type Instance is new Queue_Base with record
      null; -- Nothing more needed, just adding methods.
   end record;

end Circular_Buffer.Labeled_Queue;
