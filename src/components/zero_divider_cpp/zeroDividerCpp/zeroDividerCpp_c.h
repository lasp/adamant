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
 * @brief Creates an instance of ZeroDividerCpp class with default magic number
 * @return Pointer to the created instance
 */
ZeroDividerCpp* ZeroDividerCpp_CreateWithDefaultMagicNumber();

/**
 * @brief Creates an instance of ZeroDividerCpp class with specified magic number
 * @param magicNumber The magic number to be stored in the class
 * @return Pointer to the created instance. 
 */
ZeroDividerCpp* ZeroDividerCpp_CreateWithMagicNumber(unsigned int magicNumber);

/**
 * @brief Destroys an instance of ZeroDividerCpp class
 * @param self Pointer to the instance to be destroyed
 */
void ZeroDividerCpp_Destroy(ZeroDividerCpp *self);

/**
 * @brief Checks if the provided magic number matches the magic number stored in the class
 * @param self Pointer to the instance of ZeroDividerCpp class
 * @param magicNumber The magic number to check against the stored value
 * @return True if the magic numbers match, false otherwise
 */
boolean_32 ZeroDividerCpp_CheckMagicNumber(ZeroDividerCpp *self, int magicNumber);

/**
 * @brief Divides the magic number stored in the class by zero.
 * @param self Pointer to the instance of ZeroDividerCpp class.
 * @return Result of division.
 */
int ZeroDividerCpp_DivideByZero(ZeroDividerCpp *self);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ZERO_DIVIDER_CPP_C_H
