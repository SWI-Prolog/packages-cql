################################################################
# SWI-Prolog CQL package
# Author:    Mike Elston, Matt Lilley
# Copyright: GPL (see COPYING or www.gnu.org)
################################################################

PACKAGE=cql
DOC=cql
include ../Makefile.defs
EXDIR=$(DESTDIR)$(PKGEXDIR)/cql
PKGPLLIBDIR=$(PLBASE)/library/cql

LIBPL=		cql.pl sql_parser.pl sql_tokenizer.pl sql_write.pl sql_keywords.pl cql_database.pl cql_autoschema.pl cql_hooks.pl cql_demo.pl

all::
		@echo "Nothing to be done for this package"

install:	$(LIBPL)
		mkdir -p $(DESTDIR)$(PKGPLLIBDIR)
		for f in $(LIBPL); do \
		  $(INSTALL_DATA) $$f $(DESTDIR)$(PKGPLLIBDIR); \
		done
		$(MKINDEX)

ln-install::
		$(MAKE) INSTALL_DATA="../ln-install" INSTALL_PROGRAM="../ln-install" install

rpm-install:	install

pdf-install:
		$(INSTALL_DATA) $(DOC).pdf $(DESTDIR)$(PKGDOCDIR)

html-install:
		$(INSTALL_DATA) $(DOC).html $(DOCIMG) $(DESTDIR)$(PKGDOCDIR)

uninstall:
		(cd $(PLLIBDIR) && rm -f $(LIBPL))


################################################################
# Documentation
################################################################

TEXEXTRA=	cql.tex
$(TEX):		$(TEXEXTRA)

cql.tex:	cql.pl
		$(PLTOTEX) --subsection --out=cql.tex 'library(cql)'

################################################################
# Clean
################################################################

clean:
		rm -f *~ *% config.log
		rm -f cql.tex

distclean:	clean
		rm -f config.h config.cache config.status Makefile
		rm -rf autom4te.cache

