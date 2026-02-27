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
         * @brief Default magic number for class construction
         */
        static constexpr unsigned int DEFAULT_MAGIC_NUMBER = 42;

        /**
         * @brief Constructor for ZeroDividerCpp class
         * @param magicNumber The magic number to be stored in the class
         */
        explicit ZeroDividerCpp(unsigned int magicNumber = DEFAULT_MAGIC_NUMBER) : p_magicNumber(magicNumber) {}

        /**
         * @brief Checks if the provided magic number matches the stored magic number
         * @param magicNumber The magic number to check against the stored value
         * @return True if the magic numbers match, false otherwise
         */
        boolean_32 CheckMagicNumber(unsigned int magicNumber);
        
        /**
         * @brief Divides the given magic number by zero
         * @return Result of division
         */
        int DivideByZero(void);
    
    private:
        /**
         * @brief A private stored magic number
         */
        unsigned int p_magicNumber;
};

#endif // ZERO_DIVIDER_CPP_H