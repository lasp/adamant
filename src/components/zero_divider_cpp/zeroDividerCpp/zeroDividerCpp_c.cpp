#include "zeroDividerCpp.h"
#include "zeroDividerCpp_c.h"

ZeroDividerCpp* ZeroDividerCpp_CreateWithDefaultMagicNumber()
{
    return reinterpret_cast<ZeroDividerCpp*>(new ZeroDividerCpp());
}

ZeroDividerCpp* ZeroDividerCpp_CreateWithMagicNumber(unsigned int magicNumber)
{
    return reinterpret_cast<ZeroDividerCpp*>(new ZeroDividerCpp(magicNumber));
}

void ZeroDividerCpp_Destroy(ZeroDividerCpp *self)
{
    delete reinterpret_cast<ZeroDividerCpp*>(self);
}

boolean_32 ZeroDividerCpp_CheckMagicNumber(ZeroDividerCpp *self, int magicNumber)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->CheckMagicNumber(magicNumber);
}

int ZeroDividerCpp_DivideByZero(ZeroDividerCpp *self)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->DivideByZero();
}
