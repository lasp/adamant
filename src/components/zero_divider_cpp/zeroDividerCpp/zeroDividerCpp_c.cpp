#include "zeroDividerCpp.h"
#include "zeroDividerCpp_c.h"

ZeroDividerCpp* ZeroDividerCpp_Create(unsigned int magicNumber)
{
    return reinterpret_cast<ZeroDividerCpp*>(new ZeroDividerCpp(magicNumber));
}

boolean_32 ZeroDividerCpp_CheckMagicNumber(ZeroDividerCpp *self, unsigned int magicNumber)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->CheckMagicNumber(magicNumber);
}

int ZeroDividerCpp_IntDivideByZero(ZeroDividerCpp *self, int dividend)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->IntDivideByZero(dividend);
}

float ZeroDividerCpp_FpDivideByZero(ZeroDividerCpp *self, float dividend)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->FpDivideByZero(dividend);
}

void ZeroDividerCpp_RaiseException(ZeroDividerCpp *self)
{
    reinterpret_cast<ZeroDividerCpp*>(self)->RaiseException();
}
