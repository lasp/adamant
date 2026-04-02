pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces; use Interfaces;

package Zerodividercpp_C_H is

  --*
  -- * @brief Opaque handle to ZeroDividerCpp C++ class
  --

   type Zerodividercpp is null record;   -- incomplete struct

  --*
  -- * @brief boolean type definition for C compatibility
  --

   type Boolean_32 is (False, True) with
     Convention => C;  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:19

  --*
  -- * @brief Creates an instance of ZeroDividerCpp class with specified magic number
  -- * @param magicNumber The magic number to be stored in the class
  -- * @return Pointer to the created instance.
  -- * @note No corresponding destroy function is provided. The allocated instance is
  -- *       intentionally never freed, as this component is used exclusively for testing
  -- *       the Last Chance Handler, where recovery without a full reset is not supported.
  --

   function Zerodividercpp_Create
     (Magicnumber : Unsigned_32)
      return access Zerodividercpp
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_Create";

  --*
  -- * @brief Checks if the provided magic number matches the magic number stored in the class
  -- * @param self Pointer to the instance of ZeroDividerCpp class
  -- * @param magicNumber The magic number to check against the stored value
  -- * @return True if the magic numbers match, false otherwise
  --

   function Zerodividercpp_Checkmagicnumber
     (Self : access Zerodividercpp;
      Magicnumber : Unsigned_32)
      return Boolean_32
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_CheckMagicNumber";

  --*
  -- * @brief Divides the given dividend by zero using integer arithmetic.
  -- * @param self Pointer to the instance of ZeroDividerCpp class.
  -- * @param dividend The integer value to divide by zero.
  -- * @return Result of division.
  --

   function Zerodividercpp_Intdividebyzero
     (Self : access Zerodividercpp;
      Dividend : Integer_32)
      return Integer_32
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_IntDivideByZero";

  --*
  -- * @brief Divides the given dividend by zero using floating-point arithmetic.
  -- * @param self Pointer to the instance of ZeroDividerCpp class.
  -- * @param dividend The floating-point value to divide by zero.
  -- * @return Result of floating-point division (IEEE 754 infinity).
  --

   function Zerodividercpp_Fpdividebyzero
     (Self : access Zerodividercpp;
      Dividend : Short_Float)
      return Short_Float
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_FpDivideByZero";

  --*
  -- * @brief Raises a standard exception.
  -- * @param self Pointer to the instance of ZeroDividerCpp class.
  --

   procedure Zerodividercpp_Raiseexception
     (Self : access Zerodividercpp)
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_RaiseException";

  -- extern "C"
end Zerodividercpp_C_H;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
