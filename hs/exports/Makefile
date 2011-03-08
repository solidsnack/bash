
HS_LIB=/usr/lib/ghc-6.12.1/
HS_LIB_GHCPRIM=/usr/lib/ghc-6.12.1/ghc-prim-0.2.0.0/
HS_LIB_BASE=/usr/lib/ghc-6.12.1/base-4.2.0.0/
HS_LIB_INT=/usr/lib/ghc-6.12.1/integer-gmp-0.2.0.0/
HS_INCLUDE=/usr/lib/ghc-6.12.1/include/

exportsinit.o: exportsinit.c
	gcc -g -O2 -g -Wall -O2 -fPIC -Wall -fpic \
	    -I$(HS_INCLUDE) \
	    -c $<

exports.so: exports.hs
	ghc --make -fPIC -dynamic -c exports.hs
	ghc -shared exports.o exports_stub.o -o exports.so \
	    -L${HS_LIB} -lHSrts \
	    -L${HS_LIB_GHCPRIM} -lHSghc-prim-0.2.0.0 \
	    -L${HS_LIB_INT} -lHSinteger-gmp-0.2.0.0 \
	    -L${HS_LIB_BASE} -lHSbase-4.2.0.0 \
	    -lffi

import: import.c exports.so
	gcc -g -Wall -O2 -fPIC -Wall -o import \
	    -I$(HS_INCLUDE) \
	    import.c exports.so


clean:
	rm -f *_stub.c *_stub.h *.hi *.o *.so

