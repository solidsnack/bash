#include <stdio.h>
#include <dlfcn.h>

int main(int argc, char* argv[]) {
  char* e;
  char* so = argv[1];
  printf("**** %s %s\n", "trying to load", so);

  void *dl = dlopen(so, RTLD_NOW);
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

