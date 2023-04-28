
RVERSIONS ?= $(shell rim list)
RVERSION_DEFAULT ?= $(shell R.bat -s  -e 'cat(as.character(getRversion()))')

PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')
PKGROOT=../../..

export PAK_BUNDLE = false

DONE=$(patsubst %,lib/R-%.done,$(RVERSIONS))

build: $(DONE)

R-%: lib/R-%.done
	@true

lib/R-%.done: lib/%/_pak.done
	@echo "----- CLEANING INSTALLATION ($*) ---------------------------"
	rm -rf $@
	rm -rf lib/$*/pak/library/_cache
	rm -rf lib/$*/pak/library/*/help
	rm -rf lib/$*/pak/library/*/doc
	cd lib/$* && zip -r pak_$(PAKVERSION).zip pak
	touch $@

lib/%/pak:
	@echo "----- INSTALLING pak ($*) ----------------------------------"
	rm -rf $@
	mkdir -p lib/$*
	"C:\Program Files\R\R-$*\bin\R.exe" CMD INSTALL -l lib/$* $(PKGROOT)

lib/%/pkgdepends: lib/%/pak
	@echo "----- INSTALLING pkgdepends ($*) ---------------------------"
	rm -rf $@
	R_LIBS="lib/$*" "C:\Program Files\R\R-$*\bin\R.exe" -q -e \
		"options(repos = 'https://cran.rstudio.com');install.packages('crayon', type = 'source')"
	R_LIBS="lib/$*" "C:\Program Files\R\R-$*\bin\R.exe" -q -e \
		"pak:::safe_cran_install('pkgdepends', lib = 'lib/$*', exclude = 'crayon')"

lib/%/_pak.done: lib/%/pkgdepends
	@echo "----- INSTALLING pak dependences ($*) ----------------------"
	rm -rf $@
	R_LIBS="lib/$*" "C:\Program Files\R\R-$*\bin\R.exe" -q -e \
		"get('embed_lib', asNamespace('pak'))('$(PKGROOT)')"
	touch $@

# ------------------------------------------------------------------------

TESTS=$(patsubst %,lib/R-%.tested,$(RVERSIONS))

test: $(TESTS)

lib/R-%.tested: lib/%/_pak.done
	@echo "----- TESTING ($*) -----------------------------------------"
	rm -rf $@
	echo TODO
	touch $@

# ------------------------------------------------------------------------

R="C:\Program Files\R\R-$(RVERSION_DEFAULT)\bin\R.exe"
RLIB=lib/$(RVERSION_DEFAULT)
export PKGFILES := $(patsubst %,lib/%/pak_$(PAKVERSION).zip,$(RVERSIONS))

deploy: test
	@echo "----- DEPLOYING packages -----------------------------------"
	@echo $(PKGFILES)
	R_LIBS=$(RLIB) $(R) -q -e "library(pak);pkg_install('deps::.', dependencies = TRUE)"
	R_LIBS=$(RLIB) $(R) -q -e \
		"pak:::push_packages(strsplit(Sys.getenv('PKGFILES'), ' ')[[1]])"

# ------------------------------------------------------------------------

clean:
	rm -rf src opt lib *.tar.gz *.tar.bz2

.PRECIOUS: lib/%/pak lib/%/pkgdepends lib/%/_pak.done lib/R-%.done \
	   lib/R-%.tested

all: build deploy
