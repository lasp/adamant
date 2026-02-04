#include "zeroDividerCpp.h"

int ZeroDividerCpp::DivideByZero(int magicNumber)
{
    volatile int zero = 0;
    return magicNumber / zero;
}