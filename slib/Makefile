# Makefile for Scheme Library
# Copyright (C) 1991, 1992, 1993, 1994, 1995 Aubrey Jaffer.

SHELL = /bin/sh
intro:
	@echo
	@echo "Welcome to SLIB.  Read \"README\" and \"slib.info\" (or"
	@echo "\"slib.texi\") to learn how to install and use SLIB."
	@echo
	@echo
	-make slib.info

srcdir=$(HOME)/slib/
dvidir=../dvi/
dvi:	$(dvidir)slib.dvi
$(dvidir)slib.dvi:	$(srcdir)slib.texi $(dvidir)slib.fn
#	cd $(dvidir);texi2dvi $(srcdir)slib.texi
	-(cd $(dvidir);texindex slib.??)
	cd $(dvidir);tex $(srcdir)slib.texi
$(dvidir)slib.fn:
	cd $(dvidir);tex $(srcdir)slib.texi
xdvi:	$(dvidir)slib.dvi
	xdvi $(dvidir)slib.dvi

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
infodir = $(exec_prefix)/info
RUNNABLE = scheme48
LIB = $(libdir)/$(RUNNABLE)
VM = scheme48vm
IMAGE = slib.image

slib48:
	(echo ,load `pwd`/scheme48.init; \
	 echo "(define *args* '())"; \
	 echo "(define (program-arguments) (cons \"$(VM)\" *args*))"; \
	 echo ,dump $(LIB)/$(IMAGE); \
	 echo ,exit) | scheme48
	(echo '#!/bin/sh'; \
	 echo exec '$(LIB)/$(VM)' -i '$(LIB)/$(IMAGE)' \"\$$\@\") \
	  > $(bindir)/slib48
	chmod +x $(bindir)/slib48

info:	$(infodir)/slib.info
	-make schelog-info
$(infodir)/slib.info:	slib.texi
	makeinfo slib.texi -o $(infodir)/slib.info

infoz:	$(infodir)/slib.info.gz
	-make schelog-infoz
$(infodir)/slib.info.gz:	$(infodir)/slib.info
	-rm $(infodir)/slib.info*.gz
	gzip $(infodir)/slib.info*

#### Stuff for maintaining SLIB below ####

VERSION = 2a3
ver = $(VERSION)
ffiles = stdio.scm format.scm genwrite.scm obj2str.scm pp.scm \
	ppfile.scm strcase.scm debug.scm trace.scm lineio.scm \
	strport.scm scanf.scm chap.scm
lfiles = sort.scm comlist.scm tree.scm logical.scm random.scm
revfiles = sc4opt.scm sc4sc3.scm sc2.scm mularg.scm mulapply.scm \
	trnscrpt.scm withfile.scm dynwind.scm promise.scm values.scm
afiles = ratize.scm randinex.scm modular.scm primes.scm factor.scm \
	 charplot.scm getopt.scm time.scm cltime.scm comparse.scm
bfiles = collect.scm fluidlet.scm struct.scm structure.scm \
	object.scm recobj.scm yasyn.scm
#	yasos.scm
scfiles = r4rsyn.scm scmacro.scm synclo.scm synrul.scm synchk.scm \
	repl.scm macwork.scm mwexpand.scm mwdenote.scm mwsynrul.scm
scafiles = scainit.scm scaglob.scm scamacr.scm scaoutp.scm scaexpp.scm
dfiles = defmacex.scm mbe.scm
efiles = record.scm dynamic.scm queue.scm process.scm \
	priorque.scm hash.scm hashtab.scm alist.scm rbtree.scm \
	rbtest.scm array.scm arraymap.scm sierpinski.scm soundex.scm
rfiles = rdms.scm alistab.scm dbutil.scm paramlst.scm report.scm \
	batch.scm
gfiles = tek40.scm tek41.scm
docfiles = ANNOUNCE README FAQ ChangeLog slib.texi
mfiles = Makefile require.scm Template.scm
ifiles = chez.init elk.init macscheme.init \
	mitscheme.init scheme2c.init scheme48.init gambit.init t3.init \
	vscm.init mitcomp.pat syncase.sh
tfiles = test.scm plottest.scm formatst.scm macrotst.scm scmactst.scm \
	dwindtst.scm structst.scm mwtest.scm
sfiles = $(ffiles) $(lfiles) $(revfiles) $(afiles) $(scfiles) $(efiles) \
	$(rfiles) $(gfiles) $(scafiles) $(dfiles)
allfiles = $(docfiles) $(mfiles) $(ifiles) $(sfiles) $(tfiles) $(bfiles)

makedev = make -f $(HOME)/makefile.dev
dest = $(HOME)/dist/
temp/slib:	$(allfiles)
	-rm -rf temp
	mkdir temp
	mkdir temp/slib
	ln  $(allfiles) temp/slib

infotemp/slib:	slib.info
	-rm -rf infotemp
	mkdir infotemp
	mkdir infotemp/slib
	ln slib.info slib.info-* infotemp/slib

distinfo:	$(dest)slib.info.tar.gz
$(dest)slib.info.tar.gz:	infotemp/slib
	$(makedev) TEMP=infotemp/ DEST=$(dest) PROD=slib ver=.info tar.gz
	rm -rf infotemp

dist:	$(dest)slib$(VERSION).tar.gz
$(dest)slib$(VERSION).tar.gz:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(VERSION) tar.gz
shar:	slib.shar
slib.shar:	temp/slib
	$(makedev) PROD=slib shar
dclshar:	slib.com
com:	slib.com
slib.com:	temp/slib
	$(makedev) PROD=slib com
zip:	slib.zip
slib.zip:	temp/slib
	$(makedev) PROD=slib zip
distzip:	slib$(VERSION).zip
slib$(VERSION).zip:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(VERSION) zip
	mv $(dest)slib$(VERSION).zip /c/scm/dist/
pubzip:	temp/slib
	$(makedev) DEST=$(HOME)/pub/ PROD=slib zip

diffs:	pubdiffs
pubdiffs:	temp/slib
	$(makedev) DEST=$(HOME)/pub/ PROD=slib pubdiffs
distdiffs:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(ver) distdiffs
announcediffs:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=2a1 announcediffs

psdfiles=COPYING.psd README.psd cmuscheme.el comint.el instrum.scm pexpr.scm \
	primitives.scm psd-slib.scm psd.el read.scm runtime.scm version.scm
psdocfiles=article.bbl article.tex manual.bbl manual.tex quick-intro.tex

psdtemp/slib:
	-rm -rf psdtemp
	mkdir psdtemp
	mkdir psdtemp/slib
	mkdir psdtemp/slib/psd
	cd psd; ln $(psdfiles) ../psdtemp/slib/psd
	mkdir psdtemp/slib/psd/doc
	cd psd/doc; ln $(psdocfiles) ../../psdtemp/slib/psd/doc

psdist:	$(dest)slib-psd.tar.gz
$(dest)slib-psd.tar.gz:	psdtemp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-psd tar.gz TEMP=psdtemp/

schelogfiles = copying schelog.doc schelog.scm schelog.texi
schelogexamples = bible.scm england2.scm holland.scm mapcol.scm \
	schelogt.scm england.scm games.scm houses.scm puzzle.scm \
	toys.scm

schelogtemp/slib:
	-rm -rf schelogtemp
	mkdir schelogtemp
	mkdir schelogtemp/slib
	mkdir schelogtemp/slib/schelog
	cd schelog; ln $(schelogfiles) ../schelogtemp/slib/schelog
	mkdir schelogtemp/slib/schelog/examples
	cd schelog/examples; ln $(schelogexamples) \
	../../schelogtemp/slib/schelog/examples

schelogdist:	schelog-dist
schelog-dist:	$(dest)slib-schelog.tar.gz
$(dest)slib-schelog.tar.gz:	schelogtemp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-schelog tar.gz TEMP=schelogtemp/

schelog-info:	$(infodir)/schelog.info
$(infodir)/schelog.info:	schelog/schelog.texi
	makeinfo schelog/schelog.texi -o $(infodir)/schelog.info

schelog-infoz:	$(infodir)/schelog.info.gz
$(infodir)/schelog.info.gz:	$(infodir)/schelog.info
	-rm $(infodir)/schelog.info*.gz
	gzip $(infodir)/schelog.info*

schelog.dvi:	$(dvidir)schelog.dvi
$(dvidir)schelog.dvi:	$(srcdir)schelog/schelog.texi $(dvidir)schelog.fn
#	cd $(dvidir);texi2dvi $(srcdir)schelog/schelog.texi
	-(cd $(dvidir);texindex schelog.??)
	cd $(dvidir);tex $(srcdir)schelog/schelog.texi
$(dvidir)schelog.fn:
	cd $(dvidir);tex $(srcdir)schelog/schelog.texi
schelog-xdvi:	$(dvidir)schelog.dvi
	xdvi $(dvidir)schelog.dvi

new:
	chpat slib$(VERSION) slib$(ver) ANNOUNCE FAQ ../scm/ANNOUNCE \
		../jacal/ANNOUNCE ../wb/README ../wb/ANNOUNCE \
		/c/scm/dist/install.bat /c/scm/dist/makefile \
		../scm/README
	chpat $(VERSION) $(ver) README slib.texi require.scm Makefile

tagfiles = slib.texi $(mfiles) $(sfiles) $(bfiles) $(tfiles)
# README and $(ifiles) cause semgentation faults in ETAGS for Emacs version 19.
tags:	$(tagfiles)
	etags $(tagfiles)
test:	$(sfiles)
	scheme Template.scm $(sfiles)
rights:
	scm -ladmin -e"(admin:check-all)" $(sfiles) $(tfiles) \
		$(bfiles) $(ifiles)
clean:
	-rm -f *~ *.bak *.orig *.rej core a.out *.o \#*
	-rm -rf *temp
distclean:	realclean
realclean:
	-rm -f *~ *.bak *.orig *.rej TAGS core a.out *.o \#*
	-rm -f slib.info* slib.?? slib.???
	-rm -rf *temp
realempty:	temp/slib
	-rm -f $(allfiles)
