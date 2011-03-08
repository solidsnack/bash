#include <stdio.h>
#include "./exports_stub.h"

int main()
{
  HsWord16 num = hs42();
  printf("Hello %d\n", num);
  return 0;
}

