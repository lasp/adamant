#include "zeroDividerCpp.h"

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

int ZeroDividerCpp::DivideByZero(void)
{
    volatile unsigned int zero = 0;
    return p_magicNumber / zero;
}
