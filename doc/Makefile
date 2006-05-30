
include ../include.mk
include ../vsn.mk

IMG_EPS = $(wildcard *.eps)
IMG_PDF = $(IMG_EPS:.eps=.pdf)

all debug:	
	echo "To build the docs, please use make docs before doing make install"

docs:	yaws.ps yaws.pdf

yaws.ps:	yaws.dvi
	-dvips -o yaws.ps yaws.dvi
	-cp yaws.ps ../www

yaws.pdf:	yaws.tex $(IMG_PDF)
	-pdflatex yaws.tex
	-cp yaws.pdf ../www

yaws.dvi:	yaws.tex  $(IMG_EPS)
	-latex yaws.tex
	-latex yaws.tex

%.pdf:	%.eps
	-epstopdf $<

clean:
	-rm *~ *.ps *.dvi *.log *.aux *.pdf *.toc *.beam 2> /dev/null

install:
	-install -d $(DESTDIR)/$(PREFIX)/share/doc/yaws-$(YAWS_VSN)
	-cp yaws.pdf $(DESTDIR)/$(PREFIX)/share/doc/yaws-$(YAWS_VSN)
 
