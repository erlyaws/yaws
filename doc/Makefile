include ../include.mk


all debug:
	echo "To build the docs, please use make docs before doing make install"

yaws.ps:	yaws.dvi
	-dvips -o yaws.ps yaws.dvi
	-cp yaws.ps ../www

yaws.pdf:	yaws.dvi
	-dvipdf yaws.dvi
	-cp yaws.pdf ../www

yaws.dvi:	yaws.tex
	-latex yaws.tex

clean:
	-rm *~ *.ps *.dvi *.log *.aux *.pdf *.toc *.beam 2> /dev/null

install:
	-install -d $(INSTALLPREFIX)/share/doc/yaws
	-cp yaws.pdf $(INSTALLPREFIX)/share/doc/yaws
	-cp yaws.ps $(INSTALLPREFIX)/share/doc/yaws
