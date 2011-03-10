HS_LIB=/usr/lib/ghc-6.12.1/

libfoo.so: Foo.hs fooinit.c
	ghc -fPIC -c fooinit.c
	ghc -dynamic -fPIC -c Foo.hs
	ghc -shared Foo.o Foo_stub.o fooinit.o -o libfoo.so \
	    -L${HS_LIB} -lHSrts \
	    -L${HS_LIB}/ghc-prim-0.2.0.0/ -lHSghc-prim-0.2.0.0 \
	    -L${HS_LIB}/integer-gmp-0.2.0.0/ -lHSinteger-gmp-0.2.0.0 \
	    -L${HS_LIB}/base-4.2.0.0/ -lHSbase-4.2.0.0 \
	    -lffi -lrt

clean:
	rm -f *_stub.c *_stub.h *.hi *.o *.so

