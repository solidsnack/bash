#include "HsFFI.h"

extern void __stginit_Foo(void);
static void Foo_init (void) __attribute__ ((constructor));
void Foo_init (void) {
  hs_init(0, 0);
  hs_add_root(__stginit_Foo);
}

