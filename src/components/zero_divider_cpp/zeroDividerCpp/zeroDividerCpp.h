#ifndef ZERO_DIVIDER_CPP_H
#define ZERO_DIVIDER_CPP_H

#include "zeroDividerCpp_c.h"

/**
 * @class ZeroDividerCpp
 * @brief A class that performs division by zero
 */
class ZeroDividerCpp 
{
    public:

        /**
         * @brief Constructor for ZeroDividerCpp class
         * @param magicNumber The magic number to be stored in the class
         */
        explicit ZeroDividerCpp(unsigned int magicNumber)
            : p_magicNumber(magicNumber) {}

        /**
         * @brief Checks if the provided magic number matches the stored magic number
         * @param magicNumber The magic number to check against the stored value
         * @return True if the magic numbers match, false otherwise
         */
        boolean_32 CheckMagicNumber(unsigned int magicNumber);
        
        /**
         * @brief Divides the given dividend by zero using integer arithmetic.
         * @param dividend The integer value to divide by zero
         * @return Result of division
         */
        int IntDivideByZero(int dividend);

        /**
         * @brief Divides the given dividend by zero using floating-point arithmetic.
         * @param dividend The floating-point value to divide by zero
         * @return Result of floating-point division (IEEE 754 infinity)
         */
        float FpDivideByZero(float dividend);

        /**
         * @brief Throws a standard exception.
         */
        void RaiseException(void);

    private:
    
        /**
         * @brief A private stored magic number
         */
        unsigned int p_magicNumber;
};

#endif // ZERO_DIVIDER_CPP_H