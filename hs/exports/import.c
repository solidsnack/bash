#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "exports_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_LanguageBashExports(void);
#endif

int main(int argc, char *argv[])
{
  int i;

  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_LanguageBashExports);
#endif

  HsWord16 num = hs42();
  printf("Hello %d\n", num);

  hs_exit();
  return 0;
}

