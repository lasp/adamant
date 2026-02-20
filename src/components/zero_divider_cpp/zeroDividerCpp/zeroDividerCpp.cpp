#include "zeroDividerCpp.h"

boolean_32 ZeroDividerCpp::CheckMagicNumber(int magicNumber)
{
    if (magicNumber_ == magicNumber)
    {
        return True;
    } 
    else 
    {
        return False;
    }
}

int ZeroDividerCpp::DivideByZero(void)
{
    volatile unsigned int zero = 0;
    return magicNumber_ / zero;
}
