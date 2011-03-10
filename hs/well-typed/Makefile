HS_LIB=/usr/lib/ghc-6.12.1/

libfoo.so: Foo.hs fooinit.c
	ghc -fPIC -c fooinit.c
	ghc -dynamic -fPIC -c Foo.hs
	ghc -shared Foo.o Foo_stub.o fooinit.o -o libfoo.so \
            -L/usr/lib/ghc-6.12.1/base-4.2.0.0 \
            -L/usr/lib/ghc-6.12.1/integer-gmp-0.2.0.0 \
            -L/usr/lib/ghc-6.12.1/ghc-prim-0.2.0.0 \
            -L/usr/lib/ghc-6.12.1 \
            -lHSrtsmain \
            -lHSbase-4.2.0.0 \
            -lHSinteger-gmp-0.2.0.0 \
            -lgmp \
            -lHSghc-prim-0.2.0.0 \
            -lHSrts \
            -lm -lffi -lrt -ldl

clean:
	rm -f *_stub.c *_stub.h *.hi *.o *.so

