#include "HsFFI.h"

#define __GLASGOW_HASKELL__

extern void __stginit_LanguageBashExports(void);

void
LanguageBashExports_init(void) {
  int argc = 0;
  char **argv[] = {""};
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_LanguageBashExports);
#endif
}

void
LanguageBashExports_end(void) {
  hs_exit();
}

