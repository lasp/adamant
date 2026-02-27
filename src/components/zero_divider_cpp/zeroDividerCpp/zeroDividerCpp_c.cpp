#include "zeroDividerCpp.h"
#include "zeroDividerCpp_c.h"

ZeroDividerCpp_t* ZeroDividerCpp_CreateWithDefaultMagicNumber()
{
    return reinterpret_cast<ZeroDividerCpp_t*>(new ZeroDividerCpp());
}

ZeroDividerCpp_t* ZeroDividerCpp_CreateWithMagicNumber(unsigned int magicNumber)
{
    return reinterpret_cast<ZeroDividerCpp_t*>(new ZeroDividerCpp(magicNumber));
}

void ZeroDividerCpp_Destroy(ZeroDividerCpp_t *self)
{
    delete reinterpret_cast<ZeroDividerCpp*>(self);
}

boolean_32 ZeroDividerCpp_CheckMagicNumber(ZeroDividerCpp_t *self, int magicNumber)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->CheckMagicNumber(magicNumber);
}

int ZeroDividerCpp_DivideByZero(ZeroDividerCpp_t *self)
{
    return reinterpret_cast<ZeroDividerCpp*>(self)->DivideByZero();
}
