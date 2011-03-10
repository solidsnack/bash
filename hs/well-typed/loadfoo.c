#include <stdio.h>
#include <dlfcn.h>

int main(void) {
  char* e;
  printf("**** %s\n", "start");

  void *dl = dlopen("./libfoo.so", RTLD_NOW);
  e = dlerror();
  if ( e == NULL ) {
    printf("**** %s\n", ".so load okay");
    int (*foo)(void) = dlsym(dl, "foo");
    e = dlerror();
    if ( e == NULL ) {
      printf("**** %s\n", "symbol lookup okay");
    } else {
      printf("**** %s: %s\n", "symbol lookup error", e);
    }
  } else {
    printf("**** %s: %s\n", ".so load error", e);
  }


  return 0;
}

