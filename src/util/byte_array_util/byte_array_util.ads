with Basic_Types; use Basic_Types;

-- A collection of utility functions for managing byte arrays:
package Byte_Array_Util with SPARK_Mode => On is

   -- The contracts in this package exist for proof with GNATprove only. The
   -- assertion policy below disables them at runtime, so the generated code
   -- and the runtime behavior of this package is identical to what it was
   -- before the SPARK conversion. The defensive pragma Assert statements in
   -- the package body are not affected by this policy. They remain compiled
   -- in and enabled under the project wide assertion policy, and they are
   -- also proved.
   pragma Assertion_Policy
      (Pre => Ignore,
       Post => Ignore,
       Contract_Cases => Ignore,
       Ghost => Ignore,
       Loop_Invariant => Ignore,
       Loop_Variant => Ignore,
       Assert_And_Cut => Ignore,
       Assume => Ignore);

   -- Helper functions to determine if array is empty or not.
   function Is_Empty (A : in Byte_Array) return Boolean is (A'First > A'Last)
      with Inline => True;
   function Is_Not_Empty (A : in Byte_Array) return Boolean is (A'Last >= A'First)
      with Inline => True;

   -- Given a source array of bytes copy the right-most bytes in that array to the destination
   -- aligned to the right in the destination array. If src is bigger than dest, only
   -- the amount of bytes available in the destination are copied. If dest is bigger than source
   -- then only the amount of bytes available in the source is copied.
   procedure Safe_Right_Copy (Dest : in out Byte_Array; Src : in Byte_Array)
      with Inline => True;
   -- Same as above, but the number of bytes copied is returned:
   function Safe_Right_Copy (Dest : in out Byte_Array; Src : in Byte_Array) return Natural
      with Side_Effects,
           -- No more bytes are copied than either array holds.
           Post => Safe_Right_Copy'Result <= Dest'Length and then Safe_Right_Copy'Result <= Src'Length;

   -- Given a source array of bytes copy the left-most bytes in that array to the destination
   -- aligned to the left in the destination array. If src is bigger than dest, only
   -- the amount of bytes available in the destination are copied. If dest is bigger than source
   -- then only the amount of bytes available in the source is copied.
   procedure Safe_Left_Copy (Dest : in out Byte_Array; Src : in Byte_Array)
      with Inline => True;
   -- Same as above, but the number of bytes copied is returned:
   function Safe_Left_Copy (Dest : in out Byte_Array; Src : in Byte_Array) return Natural
      with Side_Effects,
           -- No more bytes are copied than either array holds.
           Post => Safe_Left_Copy'Result <= Dest'Length and then Safe_Left_Copy'Result <= Src'Length;

   -- Given a source byte array, extract data into a poly type (32-bits) starting at offset (in bits)
   -- and of size (in bits). If the offset and size are too large to extract from the given byte array,
   -- an error is returned, otherwise Success is returned and the extracted value is supplied in Value,
   -- right shifted as much as possible.
   --
   -- Note: This function assumes Value represents the type in big endian, i.e. MSB first, LSB last (right)
   type Extract_Poly_Type_Status is (Success, Error);
   function Extract_Poly_Type (Src : in Byte_Array; Offset : in Natural; Size : in Positive; Is_Signed : in Boolean; Value : out Poly_32_Type) return Extract_Poly_Type_Status
      with Side_Effects;

   -- Given a source poly type (32-bits) set bits in the destination byte array starting at offset
   -- (in bits) and size (in bits). The right-most (least significant) bytes in the poly type (Value)
   -- are set in the byte array if size is less than 32-bits. If offset and size are too large to set
   -- in the given byte array, an error is returned, otherwise Success is returned.
   --
   -- The Set_Poly_Type_Status is returned from the function
   --    Success - The polytype was successfully stored in the destination byte array
   --    Truncation_Error - If truncation is not allowed (i.e. Truncation_Allowed = False) then this can occur. This means
   --                                 that the provided polytype value exceeds the value representable in a type with Size size in
   --                                 bytes. This means that some information will be lost during the "set" operation. If this is
   --                                 intended, then Truncation_Allowed must be set to True. In this case, Truncation_Error will
   --                                 never be returned from the function.
   --   Error - The Offset and Size do not fit in the provided destination byte array and thus the "set" operation cannot
   --               be performed.
   --
   -- Note: This function assumes Value represents the type in big endian, i.e. MSB first, LSB last (right)
   type Set_Poly_Type_Status is (Success, Error, Truncation_Error);
   function Set_Poly_Type (Dest : in out Byte_Array; Offset : in Natural; Size : in Positive; Value : in Poly_32_Type; Truncation_Allowed : Boolean := False) return Set_Poly_Type_Status
      with Side_Effects;

end Byte_Array_Util;
