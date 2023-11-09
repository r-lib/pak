
# Need the `tr` part because of a rig bug, \ is not escaped
RVERSIONS ?= $(shell rig list --json | tr '\\' '/' | jq -r "map ( .name ) | .[]")
RVERSION_DEFAULT ?= $(shell R.bat -s  -e 'cat(as.character(getRversion()))')

PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')
PKGROOT=../../..

export PAK_BUNDLE = false

DONE=$(patsubst %,lib/R-%.done,$(RVERSIONS))

build: $(DONE)

R-%: lib/R-%.done
	@true

R="C:\Program Files\R\R-$(RVERSION_DEFAULT)\bin\R.exe"
pak_$(PAKVERSION).tar.gz:
	$(R) CMD build $(PKGROOT)

lib/R-%.done: pak_$(PAKVERSION).tar.gz
	@echo "----- CLEANING INSTALLATION ($*) ---------------------------"
	rm -rf $@
	mkdir -p lib/$*
	"C:\Program Files\R\R-$*\bin\R.exe" CMD INSTALL -l lib/$* \
		pak_$(PAKVERSION).tar.gz
	rm -rf lib/$*/pak/library/_cache
	rm -rf lib/$*/pak/library/*/help
	rm -rf lib/$*/pak/library/*/doc
	cd lib/$* && zip -r pak_$(PAKVERSION).zip pak
	touch $@

# ------------------------------------------------------------------------

RLIB=lib/$(RVERSION_DEFAULT)
export PKGFILES := $(patsubst %,lib/%/pak_$(PAKVERSION).zip,$(RVERSIONS))

deploy:
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
