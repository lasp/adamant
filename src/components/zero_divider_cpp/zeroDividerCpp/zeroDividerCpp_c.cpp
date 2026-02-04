#include "zeroDividerCpp.h"
#include "zeroDividerCpp_c.h"

int ZeroDividerCpp_DivideByZero(int magicNumber)
{
    ZeroDividerCpp_t* self;
    return reinterpret_cast<::ZeroDividerCpp*>(self)->DivideByZero(magicNumber);
}