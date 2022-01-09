IMG_EPS = a.eps b.eps layout.eps yaws_head.eps
IMG_PDF = $(IMG_EPS:.eps=.pdf)
SOURCE_DATE_EPOCH = @SOURCE_DATE_EPOCH@

EXTRA_DIST = $(IMG_EPS) overview.edoc README.rss yaws.tex

if CAN_BUILD_DOC

doc: yaws.ps yaws.pdf

install-data-local: doc
	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(docdir)
	$(AM_V_at)$(INSTALL) -m 644 yaws.pdf $(DESTDIR)$(docdir)

	$(AM_V_at)$(INSTALL) -d $(DESTDIR)$(wwwdir)
	$(AM_V_at)$(INSTALL) -m 644 yaws.ps $(DESTDIR)$(wwwdir)
	$(AM_V_at)$(INSTALL) -m 644 yaws.pdf $(DESTDIR)$(wwwdir)

uninstall-local:
	$(AM_V_at)rm -f $(DESTDIR)$(docdir)/yaws.pdf
	$(AM_V_at)rm -f $(DESTDIR)$(wwwdir)/yaws.ps
	$(AM_V_at)rm -f $(DESTDIR)$(wwwdir)/yaws.pdf

clean-local:
	$(AM_V_at)rm -f *.ps *.dvi *.log *.aux *.pdf *.toc *.out

else

doc:
	@echo "SKIPPED:"
	@if [ -z "$(DVILATEX)" ]; then 					\
		echo "  latex required to build documentation";		\
	fi
	@if [ -z "$(DVIPS)" ]; then 					\
		echo "  dvips required to build documentation";		\
	fi
	@if [ -z "$(PDFLATEX)" ]; then 				 	\
		echo "  pdflatex required to build documentation"; 	\
	fi
	@if [ -z "$(EPSTOPDF)" ]; then 				 	\
		echo "  epstopdf required to build documentation"; 	\
	fi

install-data-local: doc

endif

# For reproducible builds, SOURCE_DATE_EPOCH replaces DVIPSSource date if set
yaws.ps: yaws.dvi
	$(AM_V_GEN)TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVIPS) -q -o $@ $<
	$(AM_V_at)if [ x"$(SOURCE_DATE_EPOCH)" != "x" ]; then \
	  code='{{Y,Mo,D},{H,Mi,S}} = calendar:system_time_to_local_time('$(SOURCE_DATE_EPOCH)', 1),' ; \
	  code="$$code "'io:format("~4.4w~2.2.0w~2.2.0w:~2.2.0w~2.2.0w~2.2.0w\n", [Y,Mo,D,H,Mi,S]).' ; \
	  newdate=`$(ERL) -noshell -noinput -eval "$$code" -s init stop` || exit 1; \
	  mv -f $@ $(@).tmp || exit 1 ; \
	  sed "s/^\(%DVIPSSource:\s*TeX output\).*/\1 $$newdate/g" $(@).tmp > $@ ; \
	  res=$$? ; \
	  rm -f $(@).tmp ; \
	  exit $$res ; \
	fi

yaws.pdf: yaws.tex $(IMG_PDF)
	$(AM_V_GEN)TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(PDFLATEX) yaws.tex > /dev/null
	$(AM_V_at)TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH)  \
		$(PDFLATEX) yaws.tex > /dev/null

yaws.dvi: yaws.tex $(IMG_EPS)
	$(AM_V_GEN)TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVILATEX) yaws.tex > /dev/null
	$(AM_V_at)TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVILATEX) yaws.tex > /dev/null

.eps.pdf:
	$(AM_V_GEN)SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(EPSTOPDF) --outfile=$@ $< > /dev/null

.NOTPARALLEL:
# Local Variables:
#    tab-width: 8
# End:
