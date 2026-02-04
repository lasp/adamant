#ifndef ZERO_DIVIDER_CPP_C_H
#define ZERO_DIVIDER_CPP_C_H

#ifdef __cplusplus
extern "C" {
#endif      

/**
 * @brief Opaque handle to ZeroDividerCpp C++ class
 */
typedef struct ZeroDividerCpp ZeroDividerCpp_t;

/**
 * @brief C Wrapper for the DivideByZero method.
 * @param magicNumber The magic number to be used in division.
 * @return Result of division.
 */
int ZeroDividerCpp_DivideByZero(int magicNumber);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ZERO_DIVIDER_CPP_C_H