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
	@if [ x"$(SOURCE_DATE_EPOCH)" == x ]; then \
	  if [ x'$(AM_V_GEN)' != x ]; then \
	    echo '  $(DVIPS)     $<' ; \
	  else \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(DVIPS) -q -o $@ $< ; \
	  fi; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(DVIPS) -q -o $@ $< ; \
	else \
	  if [ x'$(AM_V_GEN)' != x ]; then \
	    echo '  $(DVIPS)     $<' ; \
	  else \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVIPS) -q -o $@ $< ; \
	  fi; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVIPS) -q -o $@ $< ; \
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
	@if [ x"$(SOURCE_DATE_EPOCH)" != x ]; then \
	  if [ x'$(AM_V_GEN)' != x ]; then \
	    echo '  $(PDFLATEX)  $<' ; \
	  else \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(PDFLATEX) $< ; \
	  fi ; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(PDFLATEX) $< > /dev/null; \
	  if [ x'$(AM_V_at)' = x ]; then \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH)  \
		$(PDFLATEX) $< ; \
	  fi ; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH)  \
		$(PDFLATEX) $< > /dev/null; \
	else \
	  if [ x'$(AM_V_GEN)' != x ]; then \
	    echo '  $(PDFLATEX)  $<' ; \
	  else \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(PDFLATEX) $< ; \
	  fi ; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(PDFLATEX) $< > /dev/null; \
	  if [ x'$(AM_V_at)' = x ]; then \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(PDFLATEX) $< ; \
	  fi ; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(PDFLATEX) $< > /dev/null; \
	fi

yaws.dvi: yaws.tex $(IMG_EPS)
	@if [ x"$(SOURCE_DATE_EPOCH)" != x ]; then \
	  if [ x'$(AM_V_at)' = x ]; then \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVILATEX) $< ; \
	  fi ; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(DVILATEX) $< > /dev/null; \
	else \
	  if [ x'$(AM_V_at)' = x ]; then \
	    echo TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(DVILATEX) $< ; \
	  fi ; \
	  TEXINPUTS="$${TEXTINPUTS}:@srcdir@" \
		$(DVILATEX) $< > /dev/null; \
	fi

.eps.pdf:
	@if [ x"$(SOURCE_DATE_EPOCH)" != x ]; then \
	  if [ x'$(AM_V_at)' = x ]; then \
	    echo SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(EPSTOPDF) --outfile=$@ $< ; \
	  fi ; \
	  SOURCE_DATE_EPOCH=$(SOURCE_DATE_EPOCH) \
		$(EPSTOPDF) --outfile=$@ $< > /dev/null; \
	else \
	  if [ x'$(AM_V_at)' = x ]; then \
	    echo $(EPSTOPDF) --outfile=$@ $< ; \
	  fi ; \
	  $(EPSTOPDF) --outfile=$@ $< > /dev/null; \
	fi

.NOTPARALLEL:
# Local Variables:
#    tab-width: 8
# End:
