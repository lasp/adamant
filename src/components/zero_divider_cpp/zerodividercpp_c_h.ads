pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces; use Interfaces;

package Zerodividercpp_C_H is

  --*
  -- * @brief Opaque handle to ZeroDividerCpp C++ class
  --

   type Zerodividercpp is null record;   -- incomplete struct

   subtype Zerodividercpp_T is Zerodividercpp;  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:11

  --*
  -- * @brief boolean type definition for C compatibility
  --

   type Boolean_32 is (False, True) with
     Convention => C;  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:19

  --*
  -- * @brief Creates an instance of ZeroDividerCpp class with default magic number.
  -- * @return Pointer to the created instance.
  --

   function Zerodividercpp_Createwithdefaultmagicnumber
      return access Zerodividercpp_T  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:25
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_CreateWithDefaultMagicNumber";

  --*
  -- * @brief Creates an instance of ZeroDividerCpp class with specified magic number.
  -- * @param magicNumber The magic number to be stored in the class.
  -- * @return Pointer to the created instance.
  --

   function Zerodividercpp_Createwithmagicnumber
     (Magicnumber : Unsigned_32)
      return access Zerodividercpp_T  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:32
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_CreateWithMagicNumber";

  --*
  -- * @brief Destroys an instance of ZeroDividerCpp class.
  -- * @param self Pointer to the instance to be destroyed.
  --

   procedure Zerodividercpp_Destroy
     (Self : access Zerodividercpp_T)  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:38
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_Destroy";

  --*
  -- * @brief C Wrapper for the CheckMagicNumber method.
  -- * @param self Pointer to the instance of ZeroDividerCpp class.
  -- * @param magicNumber The magic number to check against the stored value.
  -- * @return True if the magic numbers match, false otherwise.
  --

   function Zerodividercpp_Checkmagicnumber
     (Self : access Zerodividercpp_T;
      Magicnumber : Unsigned_32)
      return Boolean_32  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:46
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_CheckMagicNumber";

  --*
  -- * @brief C Wrapper for the DivideByZero method.
  -- * @param self Pointer to the instance of ZeroDividerCpp class.
  -- * @return Result of division.
  --

   function Zerodividercpp_Dividebyzero
     (Self : access Zerodividercpp_T)
      return Unsigned_32  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:53
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_DivideByZero";

  -- extern "C"
end Zerodividercpp_C_H;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
