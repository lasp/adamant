with Memory_Region;

-- Pack and unpack a byte array pointer into a packed record or back
-- into a byte array pointer. This is included in a separate package
-- to remove the dependency of Memory_Region from the Byte_Array_Pointer
-- package, which was causing issues in documentation generation.
package Byte_Array_Pointer.Packed is

   -- Return a packed version of this type. This is useful if you need to send
   -- this pointer down in an event or something similiar.
   function Pack (Self : in Instance) return Memory_Region.T;

   -- Opposite of above function:
   function Unpack (Region : in Memory_Region.T) return Instance;

end Byte_Array_Pointer.Packed;
