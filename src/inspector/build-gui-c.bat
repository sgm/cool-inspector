cls
cd ..
REM ./build-c.bat

cd ./inspector
ocamlc -c -I .. -I +site-lib/lablgtk2 file.ml
ocamlc -c -I .. strings.ml
ocamlc -c -I .. -I +site-lib/lablgtk2 lang.ml
ocamlc -c -I .. -I +site-lib/lablgtk2 icons.ml
ocamlc -c -I .. -I +site-lib/lablgtk2 -I +camlp4 ui.ml

ocamlc -o cinspect-c.exe -I +camlp4 dynlink.cma camlp4lib.cma ^
-I +site-lib/lablgtk2 lablgtk.cma gtkInit.cmo lablgtksourceview2.cma ^
../support.cmo ../sig.cmo ../token.cmo ../lexer.cmo ../store.cmo ../environment.cmo ../ast.cmo ../error.cmo ../native.cmo ../gram.cmo ../parser.cmo ../type.cmo ../eval.cmo ../printty.cmo ^
strings.cmo lang.cmo file.cmo icons.cmo ui.cmo

cd ..
IF NOT X%1==X del *.cmi *.cmo *.cmx *.o lexer.ml

cd ./inspector
IF NOT X%1==X del *.cmi *.cmo *.cmx *.o
