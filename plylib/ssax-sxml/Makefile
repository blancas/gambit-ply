# The Makefile is generated automatically

# If your Gambit-C is installed to a different directory,
# you may need to change the following line accordingly
prefix=/usr/local/Gambit-C
includedir=$(prefix)/include
libdir=$(prefix)/lib

INCLUDES=-I$(includedir) -L$(libdir)

all: ssax-sxml.so example.out test-sxml.out

ssax-sxml.so: ssax-sxml.c libs/gambit/common.scm libs/gambit/myenv.scm libs/srfi-13-local.scm libs/util.scm libs/gambit/parse-error.scm libs/input-parse.scm libs/look-for-str.scm ssax/char-encoding.scm ssax/SSAX-code.scm ssax/SXML-tree-trans.scm sxml-tools/sxpathlib.scm multi-parser/id/srfi-12.scm multi-parser/id/mime.scm multi-parser/id/http.scm multi-parser/id/access-remote.scm multi-parser/id/id.scm sxml-tools/xlink-parser.scm multi-parser/ssax-prim.scm multi-parser/multi-parser.scm html-prag/htmlprag.scm sxml-tools/sxml-tools.scm sxml-tools/sxpath-ext.scm sxml-tools/xpath-parser.scm sxml-tools/txpath.scm sxml-tools/sxpath.scm sxml-tools/xpath-ast.scm sxml-tools/xpath-context.scm sxml-tools/xlink.scm sxml-tools/ddo-axes.scm sxml-tools/ddo-txpath.scm sxml-tools/lazy-xpath.scm ssax/lazy-ssax.scm sxml-tools/modif.scm sxml-tools/serializer.scm sxml-tools/guides.scm stx/libmisc.scm stx/stx-engine.scm
	gcc $(INCLUDES) -shared -fPIC -D___LIBRARY -D___SHARED libs/gambit/common.c libs/gambit/myenv.c libs/srfi-13-local.c libs/util.c libs/gambit/parse-error.c libs/input-parse.c libs/look-for-str.c ssax/char-encoding.c ssax/SSAX-code.c ssax/SXML-tree-trans.c sxml-tools/sxpathlib.c multi-parser/id/srfi-12.c multi-parser/id/mime.c multi-parser/id/http.c multi-parser/id/access-remote.c multi-parser/id/id.c sxml-tools/xlink-parser.c multi-parser/ssax-prim.c multi-parser/multi-parser.c html-prag/htmlprag.c sxml-tools/sxml-tools.c sxml-tools/sxpath-ext.c sxml-tools/xpath-parser.c sxml-tools/txpath.c sxml-tools/sxpath.c sxml-tools/xpath-ast.c sxml-tools/xpath-context.c sxml-tools/xlink.c sxml-tools/ddo-axes.c sxml-tools/ddo-txpath.c sxml-tools/lazy-xpath.c ssax/lazy-ssax.c sxml-tools/modif.c sxml-tools/serializer.c sxml-tools/guides.c stx/libmisc.c stx/stx-engine.c ssax-sxml.c -o ssax-sxml.so

ssax-sxml.c common.c myenv.c srfi-13-local.c util.c parse-error.c input-parse.c look-for-str.c char-encoding.c SSAX-code.c SXML-tree-trans.c sxpathlib.c srfi-12.c mime.c http.c access-remote.c id.c xlink-parser.c ssax-prim.c multi-parser.c htmlprag.c sxml-tools.c sxpath-ext.c xpath-parser.c txpath.c sxpath.c xpath-ast.c xpath-context.c xlink.c ddo-axes.c ddo-txpath.c lazy-xpath.c lazy-ssax.c modif.c serializer.c guides.c libmisc.c stx-engine.c: sxml-tools/lazy-xpath.sch html-prag/htmlprag.sch stx/stx-engine.sch ssax/SSAX-code.sch sxml-tools/sxml-tools.sch libs/gambit/common.sch libs/gambit/myenv.sch multi-parser/id/srfi-12.sch multi-parser/id/http.sch libs/input-parse.sch libs/gambit/common.scm libs/gambit/myenv.scm libs/srfi-13-local.scm libs/util.scm libs/gambit/parse-error.scm libs/input-parse.scm libs/look-for-str.scm ssax/char-encoding.scm ssax/SSAX-code.scm ssax/SXML-tree-trans.scm sxml-tools/sxpathlib.scm multi-parser/id/srfi-12.scm multi-parser/id/mime.scm multi-parser/id/http.scm multi-parser/id/access-remote.scm multi-parser/id/id.scm sxml-tools/xlink-parser.scm multi-parser/ssax-prim.scm multi-parser/multi-parser.scm html-prag/htmlprag.scm sxml-tools/sxml-tools.scm sxml-tools/sxpath-ext.scm sxml-tools/xpath-parser.scm sxml-tools/txpath.scm sxml-tools/sxpath.scm sxml-tools/xpath-ast.scm sxml-tools/xpath-context.scm sxml-tools/xlink.scm sxml-tools/ddo-axes.scm sxml-tools/ddo-txpath.scm sxml-tools/lazy-xpath.scm ssax/lazy-ssax.scm sxml-tools/modif.scm sxml-tools/serializer.scm sxml-tools/guides.scm stx/libmisc.scm stx/stx-engine.scm
	gsc -o ssax-sxml.c libs/gambit/common.scm libs/gambit/myenv.scm libs/srfi-13-local.scm libs/util.scm libs/gambit/parse-error.scm libs/input-parse.scm libs/look-for-str.scm ssax/char-encoding.scm ssax/SSAX-code.scm ssax/SXML-tree-trans.scm sxml-tools/sxpathlib.scm multi-parser/id/srfi-12.scm multi-parser/id/mime.scm multi-parser/id/http.scm multi-parser/id/access-remote.scm multi-parser/id/id.scm sxml-tools/xlink-parser.scm multi-parser/ssax-prim.scm multi-parser/multi-parser.scm html-prag/htmlprag.scm sxml-tools/sxml-tools.scm sxml-tools/sxpath-ext.scm sxml-tools/xpath-parser.scm sxml-tools/txpath.scm sxml-tools/sxpath.scm sxml-tools/xpath-ast.scm sxml-tools/xpath-context.scm sxml-tools/xlink.scm sxml-tools/ddo-axes.scm sxml-tools/ddo-txpath.scm sxml-tools/lazy-xpath.scm ssax/lazy-ssax.scm sxml-tools/modif.scm sxml-tools/serializer.scm sxml-tools/guides.scm stx/libmisc.scm stx/stx-engine.scm
	mv common.c libs/gambit/
	mv myenv.c libs/gambit/
	mv srfi-13-local.c libs/
	mv util.c libs/
	mv parse-error.c libs/gambit/
	mv input-parse.c libs/
	mv look-for-str.c libs/
	mv char-encoding.c ssax/
	mv SSAX-code.c ssax/
	mv SXML-tree-trans.c ssax/
	mv sxpathlib.c sxml-tools/
	mv srfi-12.c multi-parser/id/
	mv mime.c multi-parser/id/
	mv http.c multi-parser/id/
	mv access-remote.c multi-parser/id/
	mv id.c multi-parser/id/
	mv xlink-parser.c sxml-tools/
	mv ssax-prim.c multi-parser/
	mv multi-parser.c multi-parser/
	mv htmlprag.c html-prag/
	mv sxml-tools.c sxml-tools/
	mv sxpath-ext.c sxml-tools/
	mv xpath-parser.c sxml-tools/
	mv txpath.c sxml-tools/
	mv sxpath.c sxml-tools/
	mv xpath-ast.c sxml-tools/
	mv xpath-context.c sxml-tools/
	mv xlink.c sxml-tools/
	mv ddo-axes.c sxml-tools/
	mv ddo-txpath.c sxml-tools/
	mv lazy-xpath.c sxml-tools/
	mv lazy-ssax.c ssax/
	mv modif.c sxml-tools/
	mv serializer.c sxml-tools/
	mv guides.c sxml-tools/
	mv libmisc.c stx/
	mv stx-engine.c stx/

example.out: ssax-sxml.so example.c example_.c
	gcc $(INCLUDES) example.c example_.c ssax-sxml.so -lgambc -lm -lutil -ldl -o example.out

example_.c example.c: ssax-sxml.so example.sch example.scm
	gsc -l ssax-sxml example.scm

test-sxml.out: ssax-sxml.so sxml-tools/tests/xtest-lib.c sxml-tools/tests/xtest-harness.c sxml-tools/tests/xtest-maker.c sxml-tools/tests/vsxpathlib.c sxml-tools/tests/vsxpath-ext.c sxml-tools/tests/vtxpath.c sxml-tools/tests/vcontext.c sxml-tools/tests/vddo.c sxml-tools/tests/vmodif.c test-sxml.c test-sxml_.c
	gcc $(INCLUDES) sxml-tools/tests/xtest-lib.c sxml-tools/tests/xtest-harness.c sxml-tools/tests/xtest-maker.c sxml-tools/tests/vsxpathlib.c sxml-tools/tests/vsxpath-ext.c sxml-tools/tests/vtxpath.c sxml-tools/tests/vcontext.c sxml-tools/tests/vddo.c sxml-tools/tests/vmodif.c test-sxml.c test-sxml_.c ssax-sxml.so -lgambc -lm -lutil -ldl -o test-sxml.out

test-sxml_.c sxml-tools/tests/xtest-lib.c sxml-tools/tests/xtest-harness.c sxml-tools/tests/xtest-maker.c sxml-tools/tests/vsxpathlib.c sxml-tools/tests/vsxpath-ext.c sxml-tools/tests/vtxpath.c sxml-tools/tests/vcontext.c sxml-tools/tests/vddo.c sxml-tools/tests/vmodif.c test-sxml.c: ssax-sxml.so sxml-tools/tests/xtest-maker.sch sxml-tools/tests/vsxpathlib.sch sxml-tools/tests/vcontext.sch sxml-tools/tests/xtest-harness.sch sxml-tools/tests/xtest-lib.scm sxml-tools/tests/xtest-harness.scm sxml-tools/tests/xtest-maker.scm sxml-tools/tests/vsxpathlib.scm sxml-tools/tests/vsxpath-ext.scm sxml-tools/tests/vtxpath.scm sxml-tools/tests/vcontext.scm sxml-tools/tests/vddo.scm sxml-tools/tests/vmodif.scm test-sxml.scm
	gsc -l ssax-sxml sxml-tools/tests/xtest-lib.scm sxml-tools/tests/xtest-harness.scm sxml-tools/tests/xtest-maker.scm sxml-tools/tests/vsxpathlib.scm sxml-tools/tests/vsxpath-ext.scm sxml-tools/tests/vtxpath.scm sxml-tools/tests/vcontext.scm sxml-tools/tests/vddo.scm sxml-tools/tests/vmodif.scm test-sxml.scm
	mv xtest-lib.c sxml-tools/tests/
	mv xtest-harness.c sxml-tools/tests/
	mv xtest-maker.c sxml-tools/tests/
	mv vsxpathlib.c sxml-tools/tests/
	mv vsxpath-ext.c sxml-tools/tests/
	mv vtxpath.c sxml-tools/tests/
	mv vcontext.c sxml-tools/tests/
	mv vddo.c sxml-tools/tests/
	mv vmodif.c sxml-tools/tests/

#------------------------------------------------
# Extra

clean:
	rm -f libs/gambit/*.c
	rm -f libs/*.c
	rm -f multi-parser/id/*.c
	rm -f multi-parser/*.c
	rm -f html-prag/*.c
	rm -f ssax/*.c
	rm -f sxml-tools/*.c
	rm -f stx/*.c
	rm -f sxml-tools/tests/*.c
	rm -f *.c
	rm -f ssax-sxml.so
	rm -f *.out
