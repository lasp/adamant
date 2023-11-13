/* file1.c */
#include <stdio.h>
#include "file3.h"

void print_num (int num)
{
  printf ("num is %d.\n", num);
  int t = get_t();
  printf ("num plus t is %d.\n", num + t);
  return;
}
