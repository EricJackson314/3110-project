MODULES= Vector Element
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -html -stars \
	-d doc.public $(MLIS)


docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -html -stars \
	-d doc.private -inv-merge-ml-mli -m A $(MLIS) $(MLS)


clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private
