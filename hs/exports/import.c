#include <stdio.h>
#include "./exports_stub.h"
#include "./exportsinit.c"

int main()
{
  LanguageBashExports_init();
  HsWord16 num = hs42();
  LanguageBashExports_end();
  printf("Hello %d\n", num);
  return 0;
}

