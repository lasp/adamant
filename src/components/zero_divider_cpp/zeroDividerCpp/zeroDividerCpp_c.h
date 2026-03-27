#ifndef ZERO_DIVIDER_CPP_C_H
#define ZERO_DIVIDER_CPP_C_H

#ifdef __cplusplus
extern "C" {
#endif      

/**
 * @brief Opaque handle to ZeroDividerCpp C++ class
 */
typedef struct ZeroDividerCpp ZeroDividerCpp;

/**
 * @brief boolean type definition for C compatibility
 */
typedef enum { 
    False = 0, 
    True = 1 
} boolean_32;

/**
 * @brief Creates an instance of ZeroDividerCpp class with specified magic number
 * @param magicNumber The magic number to be stored in the class
 * @return Pointer to the created instance.
 * @note No corresponding destroy function is provided. The allocated instance is
 *       intentionally never freed, as this component is used exclusively for testing
 *       the Last Chance Handler, where recovery without a full reset is not supported.
 */
ZeroDividerCpp* ZeroDividerCpp_Create(unsigned int magicNumber);

/**
 * @brief Checks if the provided magic number matches the magic number stored in the class
 * @param self Pointer to the instance of ZeroDividerCpp class
 * @param magicNumber The magic number to check against the stored value
 * @return True if the magic numbers match, false otherwise
 */
boolean_32 ZeroDividerCpp_CheckMagicNumber(ZeroDividerCpp *self, unsigned int magicNumber);

/**
 * @brief Divides the given dividend by zero using integer arithmetic.
 * @param self Pointer to the instance of ZeroDividerCpp class.
 * @param dividend The integer value to divide by zero.
 * @return Result of division.
 */
int ZeroDividerCpp_IntDivideByZero(ZeroDividerCpp *self, int dividend);

/**
 * @brief Divides the given dividend by zero using floating-point arithmetic.
 * @param self Pointer to the instance of ZeroDividerCpp class.
 * @param dividend The floating-point value to divide by zero.
 * @return Result of floating-point division (IEEE 754 infinity).
 */
float ZeroDividerCpp_FpDivideByZero(ZeroDividerCpp *self, float dividend);

/**
 * @brief Raises a standard exception.
 * @param self Pointer to the instance of ZeroDividerCpp class.
 */
void ZeroDividerCpp_RaiseException(ZeroDividerCpp *self);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ZERO_DIVIDER_CPP_C_H
