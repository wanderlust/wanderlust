#
# Please specify your Emacs here.
#
EMACS	= emacs
# To install Wanderlust for XEmacs 21 or later,
# running 'make install-package' is recommended.
# 'make install-package' refers $XEMACS instead of $EMACS.
XEMACS  = xemacs
#
# Target directory to install the Wanderlust package.
# (Automatically detected if this line is unchanged.)
#
LISPDIR = NONE
#LISPDIR = /usr/local/lib/mule/site-lisp

INFODIR = NONE
#INFODIR = /usr/local/share/info

# For XEmacs package.
PACKAGEDIR = NONE

# For XEmacs or Emacs 21: directory where icon files should go.
PIXMAPDIR = NONE


################# No need to modify following lines ####################
BATCHFLAG = -batch
FLAGS     = $(BATCHFLAG) -q -no-site-file

elc:
	$(EMACS) $(FLAGS) -l WL-MK -f compile-wl-package \
		$(LISPDIR) $(PIXMAPDIR)

check:
	$(EMACS) $(BATCHFLAG) -l WL-MK -f check-wl \
		$(LISPDIR) $(PIXMAPDIR)

test:
	$(EMACS) $(FLAGS) -l WL-MK -f test-wl \
		$(LISPDIR) $(PIXMAPDIR)

update-version:
	$(EMACS) $(FLAGS) -l WL-MK -f update-version \
		$(LISPDIR) $(PIXMAPDIR)

compile-strict: clean-elc
	@args="$(FLAGS)";\
	args="$$args -L elmo -L wl";\
	echo "=============================================";\
	echo "Compiling the 1st stage-----without elc files";\
	echo "=============================================";\
	for i in `$(EMACS) $(FLAGS) -l WL-MK -f wl-examine-modules 2>/dev/null`;\
	  do\
	  j=`echo $$i| sed 's/elc$$/el/g'`;\
	  echo "$(EMACS) ARGS -f batch-byte-compile $$j";\
	  $(EMACS) $$args -f batch-byte-compile $$j;\
	  mv $$i $$j"x";\
	done;\
	for i in `echo elmo/*.elx wl/*.elx utils/*.elx`; do\
	  j=`echo $$i| sed 's/elx$$/elc/g'`;\
	  mv $$i $$j;\
	done;\
	echo "==============================================";\
	echo "Compiling the 2nd stage-----with all elc files";\
	echo "==============================================";\
	for i in `$(EMACS) $(FLAGS) -l WL-MK -f wl-examine-modules 2>/dev/null`;\
	  do\
	  j=`echo $$i| sed 's/elc$$/el/g'`;\
	  echo "$(EMACS) ARGS -f batch-byte-compile $$j";\
	  $(EMACS) $$args -f batch-byte-compile $$j;\
	done

install-elc:
	$(EMACS) $(FLAGS) -l WL-MK -f install-wl-package \
		$(LISPDIR) $(PIXMAPDIR)

uninstall-elc:
	$(EMACS) $(FLAGS) -l WL-MK -f uninstall-wl-package \
		$(LISPDIR) $(PIXMAPDIR)

clean-elc:
	rm -f wl/*.elc wl/*~ wl/auto-autoloads.el wl/custom-load.el \
		wl/wl-news.el elmo/*.elc utils/*.elc utils/hmac/lisp/*.elc

package:
	$(XEMACS) $(FLAGS) -l WL-MK -f compile-wl-package-xmas \
		$(PACKAGEDIR) $(PIXMAPDIR)

install-package:
	$(XEMACS) $(FLAGS) -l WL-MK -f install-wl-package-xmas \
		$(PACKAGEDIR) $(PIXMAPDIR)

info:
	$(EMACS) $(FLAGS) -l WL-MK -f wl-texinfo-format $(INFODIR)

install-info:
	$(EMACS) $(FLAGS) -l WL-MK -f install-wl-info $(INFODIR)

mostlyclean-info:
	rm -f doc/*~ doc/*.cp doc/*.fn doc/*.ky doc/*.pg doc/*.tp doc/*.vr \
		doc/*.cps doc/*.fns doc/*.kys doc/*.pgs doc/*.tps doc/*.vrs

clean-info: mostlyclean-info
	rm -f doc/*.info doc/*.info-*

mostlyclean-dvi:
	rm -f doc/*~ doc/*.aux doc/*.log doc/*.toc

clean-dvi: mostlyclean-dvi
	rm -f doc/*.dvi

all: elc

install: install-elc

uninstall: uninstall-elc

mostlyclean: clean-elc mostlyclean-info mostlyclean-dvi

clean: mostlyclean clean-dvi

distclean: maintainer-clean

maintainer-clean: clean clean-info
