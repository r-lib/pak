# For this to work, you need `make` and either `zip` or `7z` on the PATH.
# You can have them from RtoolsXX, e.g. at `C:\rtools43\usr\bin`
# if you use Rtools43.
# You also need rig, or set the `RVERSIONS` and `RVERSION_DEFAULT`
# environment variables.

RVERSIONS ?= $(shell rig list --plain)
RVERSION_DEFAULT ?= $(shell rig default)

# `zip` is not always there (installing it from Chocolatey is flaky), but
# 7-Zip is pre-installed on the GHA Windows runners. Both take the archive
# before the input, so the call site is the same.
ZIP ?= $(shell if command -v zip >/dev/null 2>&1; \
	then echo "zip -r"; else echo "7z a -tzip"; fi)

PAKVERSION=$(shell grep "^Version:" ../../../DESCRIPTION | tr -cd '0-9.')
PKGROOT=../../..

export PAK_BUNDLE = false

DONE=$(patsubst %,lib/R-%.done,$(RVERSIONS))

build: $(DONE)

R-%: lib/R-%.done
	@true

R=rig run -r $(RVERSION_DEFAULT)
pak_$(PAKVERSION).tar.gz:
	$(R) --cmd build $(PKGROOT)

lib/R-%.done: pak_$(PAKVERSION).tar.gz
	@echo "----- CLEANING INSTALLATION ($*) ---------------------------"
	rm -rf $@
	mkdir -p lib/$*
	rig run -r $* --cmd INSTALL -l lib/$* \
		pak_$(PAKVERSION).tar.gz
	rm -rf lib/$*/pak/library/_cache
	rm -rf lib/$*/pak/library/*/help
	rm -rf lib/$*/pak/library/*/doc
	rm -f lib/$*/pak_$(PAKVERSION).zip
	cd lib/$* && $(ZIP) pak_$(PAKVERSION).zip pak
	touch $@

# ------------------------------------------------------------------------

RLIB=lib/$(RVERSION_DEFAULT)
export PKGFILES := $(patsubst %,lib/%/pak_$(PAKVERSION).zip,$(RVERSIONS))

deploy:
	@echo "----- DEPLOYING packages -----------------------------------"
	@echo $(PKGFILES)
	R_LIBS=$(RLIB) $(R) -q -e \
		"install.packages('$(PKGROOT)/src/library/curl', type = 'source', repos = NULL)"
	R_LIBS=$(RLIB) $(R) -q -e \
		"options(repos = c(CRAN = 'https://cloud.r-project.org')); pak::pkg_install('deps::$(PKGROOT)', dependencies = 'Config/Needs/deploy')"
	R_LIBS=$(RLIB) $(R) -q -e \
		"pak:::push_packages(strsplit(Sys.getenv('PKGFILES'), ' ')[[1]])"

# ------------------------------------------------------------------------

clean:
	rm -rf src opt lib *.tar.gz *.tar.bz2

.PRECIOUS: lib/%/pak lib/%/pkgdepends lib/%/_pak.done lib/R-%.done \
	   lib/R-%.tested

all: build deploy
