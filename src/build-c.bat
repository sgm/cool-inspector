cls
@REM camlp4o pr_o.cmo pa_extend.cmo parser.ml > parser_ext.ml
@REM camlp4o pr_o.cmo pa_extend.cmo support.cmo token.cmo lexer.cmo extensions/pa_extend_symbol.cmo parser.ml > parser_ext.ml

ocamllex lexer.mll
ocamlc -g -c support.mli support.ml
ocamlc -g -I +camlp4 -c sig.ml
ocamlc -g -I +camlp4 -pp camlp4of -c token.mli token.ml
ocamlc -g -I +camlp4 -pp camlp4of -c lexer.mli lexer.ml
ocamlc -g -c store.mli store.ml
ocamlc -g -c environment.mli environment.ml
ocamlc -g -I +camlp4 -c ast.mli ast.ml
ocamlc -g -I +camlp4 -c error.ml
ocamlc -g -I +camlp4 -c native.mli native.ml
ocamlc -g -I +camlp4 -I . -c gram.mli gram.ml
ocamlc -g -I +camlp4 -pp camlp4of -c parser.mli parser.ml
ocamlc -g -I +camlp4 -c type.mli type.ml
ocamlc -g -I +camlp4 -c eval.mli eval.ml
ocamlc -g -I +camlp4 -c printty.mli printty.ml
ocamlc -g -I +camlp4 -c main.ml
ocamlc -g -o coolml-c.exe -I +camlp4 dynlink.cma camlp4lib.cma support.cmo sig.cmo token.cmo lexer.cmo store.cmo environment.cmo ast.cmo error.cmo native.cmo gram.cmo parser.cmo type.cmo eval.cmo printty.cmo main.cmo
IF X%1==Xext camlp4o pr_o.cmo pa_extend.cmo support.cmo token.cmo lexer.cmo extensions/pa_extend_symbol.cmo parser.ml > parser_ext.ml
IF NOT X%1==X del *.cmi *.cmo *.cmx *.o lexer.ml
@REM cool-c
@REM TIMEOUT 1 > nul
@REM IF NOT X%1==X del cool-c.exe
