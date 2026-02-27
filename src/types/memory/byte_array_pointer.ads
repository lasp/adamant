with Basic_Types;
with Serializer_Types;
with System;

-- Basic types used by the framework
package Byte_Array_Pointer is

   -- The pointer instance. The type is made private to
   -- prevent tampering of the internal record by accident.
   -- If you need to change the length of the array pointed
   -- to, use the "Slice" function below. If you really
   -- feel like you need access to the internal pointer,
   -- call the "Pointer" function, but you should understand
   -- what you are doing.
   type Instance is private;

   -- Procedures for creating / destroying a pointer on the heap
   -- using "new":
   function Create_On_Heap (Size : in Natural; Init_To_Zero : in Boolean := False) return Instance;

   -- Create a pointer from an address and size in bytes, sizes less than zero will be set to zero:
   function From_Address (Addr : in System.Address; Size : in Integer) return Instance;

   -- Create a pointer from an access to a generic statically sized type:
   generic
      type T is private;
      type T_Access is access T;
   function From_Typed_Access (Src : in T_Access) return Instance;

   -- Create a pointer from an access to a generic variably sized type:
   generic
      type T is private;
      type T_Access is access T;
      with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status;
   function From_Variable_Length_Typed_Access (Self : out Instance; Src : in T_Access) return Serializer_Types.Serialization_Status;

   -- Return a null pointer with zero length:
   function Null_Pointer return Instance with
      Inline => True;

      -- Unreference the pointer, set it to null:
   procedure Destroy (Self : in out Instance);

   -- Create a pointer from another pointer, starting at the specified index and ending at the specified index.
   -- If the end index is not specified, then it is set to the end of the input pointer. Use this method
   -- if you only want to copy data to/from certain indexes in the underlying byte array.
   function Slice (Self : in Instance; Start_Index : in Natural; End_Index : in Integer := -1) return Instance;

   -- Is the pointer invalid (i.e. internal length is negative):
   function Is_Null (Self : in Instance) return Boolean with
      Inline => True;

   -- Get length of buffer. Will be zero or higher.
   function Length (Self : in Instance) return Natural with
      Inline => True;

   -- Get address of buffer. Using "Pointer" should be preferred, except in rare circumstances
   -- where the actual address is needed.
   function Address (Self : in Instance) return System.Address with
      Inline => True;

      -- Get pointer to byte array. Only call this function and use the
      -- result if you know what you are doing. This is an "escape hatch"
      -- function if you need to do something that is not provided by this
      -- package or related packages dealing with byte array pointers.
      -- To use this pointer correctly you MUST take a slice of the resulting
      -- array to allow Ada to check bounds for you. See the implementation of
      -- the "Copy_To" function below for an example of how to do this correctly.
   function Pointer (Self : in Instance) return Basic_Types.Unsafe_Byte_Array_Access with
      Inline => True;

      -- Convert pointer to a byte array. This effectively copies the bytes in the pointer
      -- to the byte array upon return:
   function To_Byte_Array (Self : in Instance) return Basic_Types.Byte_Array;

   -- Copy a byte array to the byte array pointer. Note: that the lengths of the
   -- pointer and the input byte must match. Use the "Slice" function above if
   -- necessary to restrict the bounds of the pointer.
   procedure Copy_To (Self : in Instance; Bytes : in Basic_Types.Byte_Array);

   -- Copy data from one byte array pointer to another. Note that the lengths
   -- of the pointers must match. Use the "Slice" function above if necessary to
   -- restrict the bounds of the pointer.
   procedure Copy (Destination : in Instance; Source : in Instance);

   -- Convert the pointer to a statically sized type. This effectively copies the
   -- bytes in the pointer to the static type via a deserializing function.
   generic
      type T is private;
      with function From_Byte_Array (Src : in Basic_Types.Byte_Array) return T;
   function To_Type (Self : in Instance) return T;

   -- Convert the pointer to a variable length type. This effectively copies the
   -- bytes in the pointer to the static type via a deserializing function.
   generic
      type T is private;
      with function From_Byte_Array (Dest : out T; Src : in Basic_Types.Byte_Array) return Serializer_Types.Serialization_Status;
   function To_Variable_Length_Type (Dest : out T; Self : in Instance) return Serializer_Types.Serialization_Status;

   -- Take a statically sized type and serialize it into the byte array pointer's data. Note: that
   -- the lengths of the pointer and the serialized type must match. User the "Slice" function above
   -- if necessary to restrict the bounds of the pointer to the expected length.
   generic
      type T is private;
      with procedure To_Byte_Array (Dest : out Basic_Types.Byte_Array; Src : in T);
   procedure Copy_From_Type (Self : in Instance; Src : in T);

   -- Take a variable length type and serialize it into the byte array pointer's data. Note: that
   -- the lengths of the pointer and the serialized type must match. User the "Slice" function above
   -- if necessary to restrict the bounds of the pointer to the expected length.
   generic
      type T is private;
      with function To_Byte_Array (Dest : out Basic_Types.Byte_Array; Src : in T) return Serializer_Types.Serialization_Status;
   function Copy_From_Variable_Length_Type (Self : in Instance; Src : in T) return Serializer_Types.Serialization_Status;

private

   type Instance is record
      Address : System.Address := System.Null_Address;
      Length : Integer := -1;
   end record;

end Byte_Array_Pointer;
