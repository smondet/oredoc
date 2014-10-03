
.PHONY: all clean byte native top plugin

all: native

MAIN=oredoc
FINDLIB_PACKAGES=omd higlo.ocaml nonstd sosa

BUILD_FLAGS=-cflags -rectypes -tag thread -use-ocamlfind 
package_options=$(foreach p, $(FINDLIB_PACKAGES), -package $(p))

byte:
	ocamlbuild $(BUILD_FLAGS) $(package_options) $(MAIN).byte && \
	  mv $(MAIN).byte $(MAIN)

native:
	ocamlbuild $(BUILD_FLAGS) $(package_options) $(MAIN).native && \
	  mv $(MAIN).native $(MAIN)


.merlin:
	echo 'B _build/' > .merlin && echo 'S .' >> .merlin && \
	  for p in $(FINDLIB_PACKAGES) ; do echo "PKG $$p" >> .merlin ; done && \
	  echo Done

clean:
	rm -fr _build $(MAIN)
