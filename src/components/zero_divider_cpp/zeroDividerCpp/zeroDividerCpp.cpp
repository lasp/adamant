#include "zeroDividerCpp.h"
#include <exception>

boolean_32 ZeroDividerCpp::CheckMagicNumber(unsigned int magicNumber)
{
    if (p_magicNumber == magicNumber)
    {
        return True;
    } 
    else 
    {
        return False;
    }
}

int ZeroDividerCpp::IntDivideByZero(int dividend)
{
    volatile int zero = 0;
    return dividend / zero;
}

float ZeroDividerCpp::FpDivideByZero(float dividend)
{
    volatile float zero = 0.0f;
    return dividend / zero;
}

void ZeroDividerCpp::RaiseException(void)
{
    throw std::exception();
}
