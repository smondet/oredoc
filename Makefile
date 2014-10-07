
.PHONY: all clean byte native top apidoc install uninstall doc

all: native

MAIN=oredoc
FINDLIB_PACKAGES=omd higlo.ocaml nonstd sosa re.posix

BUILD_FLAGS=-cflags -rectypes -tag thread -use-ocamlfind 
package_options=$(foreach p, $(FINDLIB_PACKAGES), -package $(p))

byte:
	ocamlbuild $(BUILD_FLAGS) $(package_options) $(MAIN).byte && \
	  mv $(MAIN).byte $(MAIN)

native: $(MAIN)

$(MAIN): oredoc.ml
	ocamlbuild $(BUILD_FLAGS) $(package_options) $(MAIN).native && \
	  mv _build/$(MAIN).native $(MAIN)

apidoc:
	mkdir -p _apidoc && \
	ocamlfind ocamldoc -rectypes -html -d _apidoc/ $(package_options) \
	  -thread  -charset UTF-8 -t "Oredoc API" -keep-code -colorize-code \
	  -sort -I _build/ oredoc.ml

doc: apidoc $(MAIN)
	INPUT=oredoc.ml \
	      INDEX=README.md \
	      TITLE_PREFIX="Oredoc: " \
	      OUTPUT_DIR=_doc \
	      COMMAND_SUBSTITUTIONS=oredoc:_build/oredoc.native,some_command:gcc  \
	      API=_apidoc \
	      CATCH_MODULE_PATHS=^Oredoc:,^Markdown:Oredoc. \
	      TITLE_SUBSTITUTIONS="oredoc.ml:Literate Implementation" \
	      ./$(MAIN)

.merlin:
	echo 'B _build/' > .merlin && echo 'S .' >> .merlin && \
	  for p in $(FINDLIB_PACKAGES) ; do echo "PKG $$p" >> .merlin ; done && \
	  echo Done

install: $(MAIN)
	mkdir -p $(BINDIR) &&  cp $(MAIN) $(BINDIR)/

uninstall:
	rm -f  $(BINDIR)/$(MAIN)

clean:
	rm -fr _build $(MAIN)
