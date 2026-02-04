pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;

package Zerodividercpp_C_H is

  --*
  -- * @brief Opaque handle to ZeroDividerCpp C++ class
  --

   type Zerodividercpp is null record;   -- incomplete struct

   subtype Zerodividercpp_T is Zerodividercpp;  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:11

  --*
  -- * @brief C Wrapper for the DivideByZero method.
  -- * @param magicNumber The magic number to be used in division.
  -- * @return Result of division.
  --

   function Zerodividercpp_Dividebyzero
     (Magicnumber : int)
      return int  -- /home/user/adamant/src/components/zero_divider_cpp/zeroDividerCpp/zeroDividerCpp_c.h:18
   with
     Import => True, Convention => C, External_Name => "ZeroDividerCpp_DivideByZero";

  -- extern "C"
end Zerodividercpp_C_H;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
