OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
OCAMLC = ocamlc

.PHONY: all

all: mylisp

parser.ml parser.mli:
	$(OCAMLYACC) parser.mly

lexer.ml:
	$(OCAMLLEX) lexer.mll

evaluator.cmo evaluator.cmi:
	$(OCAMLC) -c evaluator.ml

parser.cmi: parser.ml parser.mli evaluator.cmo evaluator.cmi
	$(OCAMLC) -c parser.mli

lexer.cmo lexer.cmi: lexer.ml parser.cmo parser.cmi
	$(OCAMLC) -c lexer.ml

parser.cmo: parser.ml parser.mli parser.cmi evaluator.cmo evaluator.cmi
	$(OCAMLC) -c parser.ml

mylisp.cmo mylisp.cmi:
	$(OCAMLC) -c mylisp.ml

mylisp: lexer.cmo parser.cmo evaluator.cmo mylisp.cmo
	$(OCAMLC) -o mylisp lexer.cmo parser.cmo evaluator.cmo mylisp.cmo
