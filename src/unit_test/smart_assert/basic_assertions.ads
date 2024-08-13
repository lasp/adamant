with Smart_Assert;
with Interfaces; use Interfaces;
with Basic_Types.Representation; use Basic_Types;
with System;

package Basic_Assertions is

   -- This package contains smart assert packages for common types. A unit test
   -- can simply "with" and "use" this assertions package to easily gain access to
   -- these commonly used assertion packages.
   package Natural_Assert is new Smart_Assert.Discrete (Natural, Natural'Image);
   package Integer_Assert is new Smart_Assert.Discrete (Integer, Integer'Image);
   package Positive_Assert is new Smart_Assert.Discrete (Positive, Positive'Image);
   package Unsigned_8_Assert is new Smart_Assert.Discrete (Unsigned_8, Unsigned_8'Image);
   package Unsigned_16_Assert is new Smart_Assert.Discrete (Unsigned_16, Unsigned_16'Image);
   package Unsigned_32_Assert is new Smart_Assert.Discrete (Unsigned_32, Unsigned_32'Image);
   package Unsigned_64_Assert is new Smart_Assert.Discrete (Unsigned_64, Unsigned_64'Image);
   package Integer_8_Assert is new Smart_Assert.Discrete (Integer_8, Integer_8'Image);
   package Integer_16_Assert is new Smart_Assert.Discrete (Integer_16, Integer_16'Image);
   package Integer_32_Assert is new Smart_Assert.Discrete (Integer_32, Integer_32'Image);
   package Integer_64_Assert is new Smart_Assert.Discrete (Integer_64, Integer_64'Image);
   package Float_Assert is new Smart_Assert.Float (Float, Float'Image);
   package Short_Float_Assert is new Smart_Assert.Float (Short_Float, Short_Float'Image);
   package Long_Float_Assert is new Smart_Assert.Float (Long_Float, Long_Float'Image);
   package IEEE_Float_32_Assert is new Smart_Assert.Float (IEEE_Float_32, IEEE_Float_32'Image);
   package IEEE_Float_64_Assert is new Smart_Assert.Float (IEEE_Float_64, IEEE_Float_64'Image);
   package Boolean_Assert is new Smart_Assert.Basic (Boolean, Boolean'Image);
   package Byte_Assert is new Smart_Assert.Discrete (Byte, Byte'Image);
   package Byte_Array_Assert is new Smart_Assert.Basic (Byte_Array, Basic_Types.Representation.Image);
   package Address_Assert is new Smart_Assert.Basic (System.Address, System.Address'Image);

end Basic_Assertions;
