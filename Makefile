MODULES= Vector Matrix Grid Num MatAlg Img Reader Writer Complexc
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PGS=oUnit

default: 
	opam install camlimages

demo: build
	utop -init demo.ml

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

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
	rm -rf cs3110-project.zip

zip:
	zip cs3110-project *.ml* *.md .merlin _tags Makefile
